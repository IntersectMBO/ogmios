import { dummyInteractionContext } from './helpers'
import { createChainSynchronizationClient } from '../src/ChainSynchronization'
import { networkTip, protocolParameters } from '../src/LedgerStateQuery'
import { createMempoolMonitoringClient, MempoolMonitoringClient } from '../src/MempoolMonitoring'
import { submitTransaction } from '../src/TransactionSubmission'
import { Transaction } from '@cardano-ogmios/schema'
import { execFileSync } from 'child_process'
import { inspect } from 'util'

// Set by scripts/dijkstra-devnet-test.sh. Without DIJKSTRA_TX_ID, these tests are skipped.
const transactionCbor = process.env.DIJKSTRA_TX_CBOR
const transactionId = process.env.DIJKSTRA_TX_ID
const subTransactionId = process.env.DIJKSTRA_SUB_TX_ID
const producerPid = process.env.DIJKSTRA_PRODUCER_PID

// Pause a process with SIGSTOP, and wait until it is stopped.
const pause = async (pid: number) => {
  process.kill(pid, 'SIGSTOP')
  while (!execFileSync('ps', ['-o', 'stat=', '-p', String(pid)]).toString().trim().startsWith('T')) {
    await new Promise(resolve => setTimeout(resolve, 10))
  }
}

const describeOnDevnet = transactionId === undefined ? describe.skip : describe

describeOnDevnet('Dijkstra devnet', () => {
  // Each test needs the transaction submitted, so it is submitted once, first.
  let submittedId: string | undefined
  // The next block takes the transaction out of the mempool. So the mempool
  // snapshot is taken by acquireMempool() in beforeAll, right after the submission.
  let mempool: MempoolMonitoringClient

  beforeAll(async () => {
    if (transactionCbor === undefined || producerPid === undefined) {
      throw new Error('DIJKSTRA_TX_CBOR or DIJKSTRA_PRODUCER_PID is not set: run this test through scripts/dijkstra-devnet-test.sh')
    }
    mempool = await createMempoolMonitoringClient(await dummyInteractionContext())
    const context = await dummyInteractionContext()
    try {
      // Ogmios is connected to a relay. While the block producer is paused, no
      // block can take the transaction out of the mempool before the snapshot.
      await pause(Number(producerPid))
      submittedId = await submitTransaction(context, transactionCbor)
      await mempool.acquireMempool()
    } catch (error) {
      // The errors of the client have an empty stack, which Jest shows as an empty message.
      throw new Error(`Submitting the transaction or taking the mempool snapshot failed: ${inspect(error, { depth: null })}`)
    } finally {
      context.socket.close()
      process.kill(Number(producerPid), 'SIGCONT')
    }
  })

  afterAll(async () => {
    await mempool?.shutdown()
  })

  it('submits a transaction with a sub-transaction', () => {
    expect(submittedId).toBe(transactionId)
  })

  it('reads a transaction with a sub-transaction from the mempool', async () => {
    expect(submittedId).toBeDefined()
    let tx: Transaction | null
    try {
      tx = await mempool.nextTransaction({ fields: 'all' })
      while (tx !== null && tx.id !== transactionId) {
        tx = await mempool.nextTransaction({ fields: 'all' })
      }
    } catch (error) {
      // The errors of the client have an empty stack, which Jest shows as an empty message.
      throw new Error(`Reading the mempool failed: ${inspect(error, { depth: null })}`)
    }

    expect(tx?.id).toBe(transactionId)
    expect(tx?.subTransactions?.map(subTx => subTx.id)).toEqual([subTransactionId])
  })

  it('rejects a transaction submitted twice, and keeps the connection open', async () => {
    expect(submittedId).toBeDefined()
    const context = await dummyInteractionContext()
    try {
      // 3173: all inputs of the transaction are already spent.
      await expect(submitTransaction(context, transactionCbor as string)).rejects.toMatchObject({ code: 3173 })
      await expect(networkTip(context)).resolves.toBeDefined()
    } finally {
      context.socket.close()
    }
  })

  it('reads a transaction with a sub-transaction from a block', async () => {
    // Jest 26 runs the tests even when beforeAll fails. Without a submitted
    // transaction, the blocks would be read until the test times out.
    expect(submittedId).toBeDefined()
    const context = await dummyInteractionContext()
    let found : (tx: Transaction) => void
    const transaction = new Promise<Transaction>(resolve => { found = resolve })
    const client = await createChainSynchronizationClient(context, {
      rollBackward: async (_response, nextBlock) => {
        nextBlock()
      },
      rollForward: async ({ block }, nextBlock) => {
        const tx = block.type === 'praos'
          ? block.transactions?.find(tx => tx.id === transactionId)
          : undefined
        if (tx !== undefined) {
          found(tx)
        } else {
          nextBlock()
        }
      }
    })
    await client.resume(['origin'], 1)
    try {
      const tx = await transaction

      expect(tx.id).toBe(transactionId)
      expect(tx.subTransactions?.map(subTx => subTx.id)).toEqual([subTransactionId])
    } finally {
      await client.shutdown()
    }
  })

  it('reads the protocol parameters', async () => {
    const context = await dummyInteractionContext()
    try {
      // Only the Dijkstra era has maxReferenceScriptsSizePerTransaction.
      await expect(protocolParameters(context)).resolves.toHaveProperty('maxReferenceScriptsSizePerTransaction')
    } finally {
      context.socket.close()
    }
  })
})
