import { dummyInteractionContext } from './helpers'
import { createChainSynchronizationClient } from '../src/ChainSynchronization'
import { submitTransaction } from '../src/TransactionSubmission'
import { Transaction } from '@cardano-ogmios/schema'
import { inspect } from 'util'

// Set by scripts/dijkstra-devnet-test.sh. Without DIJKSTRA_TX_ID, these tests are skipped.
const transactionCbor = process.env.DIJKSTRA_TX_CBOR
const transactionId = process.env.DIJKSTRA_TX_ID
const subTransactionId = process.env.DIJKSTRA_SUB_TX_ID

const describeOnDevnet = transactionId === undefined ? describe.skip : describe

describeOnDevnet('Dijkstra devnet', () => {
  // Each test needs the transaction submitted, so it is submitted once, first.
  let submittedId: string | undefined

  beforeAll(async () => {
    if (transactionCbor === undefined) {
      throw new Error('DIJKSTRA_TX_CBOR is not set: run this test through scripts/dijkstra-devnet-test.sh')
    }
    const context = await dummyInteractionContext()
    try {
      submittedId = await submitTransaction(context, transactionCbor)
    } catch (error) {
      // The errors of the client have an empty stack, which Jest shows as an empty message.
      throw new Error(`submitTransaction failed: ${inspect(error, { depth: null })}`)
    } finally {
      context.socket.close()
    }
  })

  it('submits a transaction with a sub-transaction', () => {
    expect(submittedId).toBe(transactionId)
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
})
