import { dummyInteractionContext } from './helpers'
import { createChainSynchronizationClient } from '../src/ChainSynchronization'
import { Transaction } from '@cardano-ogmios/schema'

// Set by scripts/dijkstra-devnet-test.sh. Without them, this test is skipped.
const transactionId = process.env.DIJKSTRA_TX_ID
const subTransactionId = process.env.DIJKSTRA_SUB_TX_ID

const describeOnDevnet = transactionId === undefined ? describe.skip : describe

describeOnDevnet('Dijkstra devnet', () => {
  it('reads a transaction with a sub-transaction from a block', async () => {
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
    const tx = await transaction
    await client.shutdown()

    expect(tx.id).toBe(transactionId)
    expect(tx.subTransactions?.map(subTx => subTx.id)).toEqual([subTransactionId])
  })
})
