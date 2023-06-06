#ifndef __PROGTEST__

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <cassert>
#include <cmath>

using namespace std;
#endif /* __PROGTEST__ */

//size of chunks: 2^31, 2^30, ..., 2^6
#define numOfChunksType 26

struct Chunk {
    int size;
    bool used;
    Chunk *next;
    Chunk *prev;

    Chunk(int size) {
        this->size = size;
        used = false;
        next = nullptr;
        prev = nullptr;
    }
};

void *start = nullptr;
int size = 0;
Chunk *chunks[numOfChunksType];
int numOfUsedChunks = 0;

void HeapInit(void *memPool, int memSize) {
    start = memPool;
    size = memSize;
    numOfUsedChunks = 0;

    for (auto &chunk : chunks) {
        chunk = nullptr;
    }

    // assign empty chunks as big as possible to array
    void *currPos = start;
    int chunkSize = INT32_MAX;
    for (int i = 0; i < numOfChunksType; i++) {
        if ((char *) currPos + chunkSize <= (char *) start + memSize) {
            chunks[i] = (Chunk *) currPos;
            *chunks[i] = Chunk(chunkSize);
            // tail size
            *(int *) ((char *) chunks[i] + chunks[i]->size - sizeof(int)) = chunks[i]->size;

            currPos = (char *) currPos + chunkSize;
        }
        if (chunkSize == INT32_MAX) chunkSize = chunkSize / 2 + 1;
        else chunkSize /= 2;
    }
}

void *HeapAlloc(int size) {
    int chunkSize = INT32_MAX;
    int lastExistingChunkType = -1;
    for (int i = 0; i < numOfChunksType; i++) {
        if ((int) (size + sizeof(Chunk) + sizeof(int)) > chunkSize) {
            if (lastExistingChunkType == -1) return nullptr;

            int chunkTypeUsed = lastExistingChunkType;
            int chunkTypeWanted = i - 1;
            while (chunkTypeUsed != chunkTypeWanted) {
                // divide chunk
                Chunk *origChunk = chunks[chunkTypeUsed];
                chunks[chunkTypeUsed] = chunks[chunkTypeUsed]->next;
                if (chunks[chunkTypeUsed]) chunks[chunkTypeUsed]->prev = nullptr;

                int newSize = 1 << (31 - (chunkTypeUsed + 1));
                Chunk *chunk1Addr = origChunk;
                Chunk *chunk2Addr = (Chunk *) ((char *) origChunk + newSize);

                *chunk1Addr = Chunk(newSize);
                *chunk2Addr = Chunk(newSize);
                *(int *) ((char *) chunk1Addr + newSize - sizeof(int)) = newSize;
                *(int *) ((char *) chunk2Addr + newSize - sizeof(int)) = newSize;

                Chunk *chunk = chunks[chunkTypeUsed + 1];
                chunks[chunkTypeUsed + 1] = chunk1Addr;
                chunk1Addr->next = chunk2Addr;
                chunk2Addr->next = chunk;
                chunk2Addr->prev = chunk1Addr;
                if (chunk) chunk->prev = chunk2Addr;

                chunkTypeUsed++;
            }

            numOfUsedChunks++;
            Chunk *retChunk = chunks[chunkTypeWanted];
            chunks[chunkTypeWanted] = chunks[chunkTypeWanted]->next;
            if (chunks[chunkTypeWanted]) chunks[chunkTypeWanted]->prev = nullptr;
            retChunk->used = true;
            retChunk->next = nullptr;
            return (char *) retChunk + sizeof(Chunk);
        }
        if (chunks[i] != nullptr) lastExistingChunkType = i;
        if (chunkSize == INT32_MAX) chunkSize = chunkSize / 2 + 1;
        else chunkSize /= 2;
    }

    return nullptr;
}

bool HeapFree(void *blk) {
    Chunk *chunk = (Chunk *) ((char *) blk - sizeof(Chunk));
    if (!chunk->used) return false;
    if (chunk->size <= (1 << (32 - numOfChunksType)) || (char *) start + chunk->size >= (char *) start + size)
        return false;
    if (chunk->size != *(int *) ((char *) chunk + chunk->size - sizeof(int))) return false;
    numOfUsedChunks--;

    int chunkType = 32 - (log2(chunk->size) + 1);
    bool merged = true;
    while (merged) {
        merged = false;
        Chunk *prevChunk = nullptr;
        Chunk *nextChunk = nullptr;
        if (chunk - 1 >= start) prevChunk = (Chunk *) ((char *) chunk - *((int *) chunk - 1));
        if (chunk + chunk->size + sizeof(Chunk) <= (void *) ((char *) start + size))
            nextChunk = (Chunk *) ((char *) chunk + chunk->size);

        Chunk *firstChunk = chunk;
        Chunk *usedNeighbour = nullptr;
        if (prevChunk && !prevChunk->used && prevChunk->size == chunk->size) {
            firstChunk = prevChunk;
            usedNeighbour = prevChunk;
        } else if (nextChunk && !nextChunk->used && nextChunk->size == chunk->size) {
            firstChunk = chunk;
            usedNeighbour = nextChunk;
        }

        if (usedNeighbour) {
            merged = true;

            if (usedNeighbour->prev == nullptr) {
                chunks[chunkType] = usedNeighbour->next;
                if (usedNeighbour->next) usedNeighbour->next->prev = nullptr;
            } else {
                usedNeighbour->prev->next = usedNeighbour->next;
                if (usedNeighbour->next) usedNeighbour->next->prev = usedNeighbour->prev;
            }

            *firstChunk = Chunk(chunk->size * 2);
            *(int *) ((char *) firstChunk + firstChunk->size - sizeof(int)) = firstChunk->size;

            chunk = firstChunk;
            chunkType--;
        } else {
            chunk->used = false;

            Chunk *frontChunkOfChunks = chunks[chunkType];
            chunks[chunkType] = firstChunk;
            firstChunk->next = frontChunkOfChunks;
            if (frontChunkOfChunks) frontChunkOfChunks->prev = firstChunk;
        }
    }

    return true;
}

void HeapDone(int *pendingBlk) {
    *pendingBlk = numOfUsedChunks;
}

#ifndef __PROGTEST__

int main(void) {
    uint8_t *p0, *p1, *p2, *p3, *p4;
    int pendingBlk;
    static uint8_t memPool[3 * 1048576];

    HeapInit(memPool, 2097152);
    assert ((p0 = (uint8_t *) HeapAlloc(512000)) != NULL);
    memset(p0, 0, 512000);
    assert ((p1 = (uint8_t *) HeapAlloc(511000)) != NULL);
    memset(p1, 0, 511000);
    assert ((p2 = (uint8_t *) HeapAlloc(26000)) != NULL);
    memset(p2, 0, 26000);
    HeapDone(&pendingBlk);
    assert (pendingBlk == 3);


    HeapInit(memPool, 2097152);
    assert ((p0 = (uint8_t *) HeapAlloc(1000000)) != NULL);
    memset(p0, 0, 1000000);
    assert ((p1 = (uint8_t *) HeapAlloc(250000)) != NULL);
    memset(p1, 0, 250000);
    assert ((p2 = (uint8_t *) HeapAlloc(250000)) != NULL);
    memset(p2, 0, 250000);
    assert ((p3 = (uint8_t *) HeapAlloc(250000)) != NULL);
    memset(p3, 0, 250000);
    assert ((p4 = (uint8_t *) HeapAlloc(50000)) != NULL);
    memset(p4, 0, 50000);
    assert (HeapFree(p2));
    assert (HeapFree(p4));
    assert (HeapFree(p3));
    assert (HeapFree(p1));
    assert ((p1 = (uint8_t *) HeapAlloc(500000)) != NULL);
    memset(p1, 0, 500000);
    assert (HeapFree(p0));
    assert (HeapFree(p1));
    HeapDone(&pendingBlk);
    assert (pendingBlk == 0);


    HeapInit(memPool, 2359296);
    assert ((p0 = (uint8_t *) HeapAlloc(1000000)) != NULL);
    memset(p0, 0, 1000000);
    assert ((p1 = (uint8_t *) HeapAlloc(500000)) != NULL);
    memset(p1, 0, 500000);
    assert ((p2 = (uint8_t *) HeapAlloc(500000)) != NULL);
    memset(p2, 0, 500000);
    assert ((p3 = (uint8_t *) HeapAlloc(500000)) == NULL);
    assert (HeapFree(p2));
    assert ((p2 = (uint8_t *) HeapAlloc(300000)) != NULL);
    memset(p2, 0, 300000);
    assert (HeapFree(p0));
    assert (HeapFree(p1));
    HeapDone(&pendingBlk);
    assert (pendingBlk == 1);


    HeapInit(memPool, 2359296);
    assert ((p0 = (uint8_t *) HeapAlloc(1000000)) != NULL);
    memset(p0, 0, 1000000);
    assert (!HeapFree(p0 + 1000));
    HeapDone(&pendingBlk);
    assert (pendingBlk == 1);


    return 0;
}

#endif /* __PROGTEST__ */

