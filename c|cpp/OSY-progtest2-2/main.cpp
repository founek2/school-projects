#ifndef __PROGTEST__

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <cassert>
#include <cmath>

using namespace std;
#endif /* __PROGTEST__ */

short NONE = 0;
short LEFT = 1;
short RIGHT = 2;

struct Block;

int *getSizeTag(Block *blk, int size);

struct Block {
    Block(int size_len, void *address, Block *next = nullptr, Block *prev = nullptr) : size(size_len),
                                                                                       next_blok(next),
                                                                                       prev_blok(prev) {
        data = sizeof(Block) + static_cast<uint8_t *>(address);

        // write size to tail
        *getSizeTag((Block *) address, size) = size;
    }

    int size;   // this way sizeof Block is 24B, when using ptr to size it went up to 32B
    bool inuse = false;
    short state = 0;
    uint8_t *data;
    Block *next_blok;
    Block *prev_blok;
};

int *getSizeTag(Block *blk, int size) {
    return (int *) ((char *) blk + size - sizeof(int));
}

struct Head {
    // Head(Block * head_): head(head_){};
    Block *used_head = nullptr;
    Block *used_first = nullptr;

    Block *free_head = nullptr;
    Block *free_first = nullptr;
};

class Bin {
    /*      LEN = 62
     * 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120, 128, 136, 144, 152, 160, 168, 176, 184, 192, 200, 208, 216,
     * 224, 232, 240, 248, 256, 264, 272, 280, 288, 296, 304, 312, 320, 328, 336, 344, 352, 360, 368, 376, 384, 392, 400,
     * 408, 416, 424, 432, 440, 448, 456, 464, 472, 480, 488, 496, 504, 512
     *      LEN = 43
     * 768, 1024, 1536, 2048, 3072, 4096, 6144, 8192, 12288, 16384, 24576, 32768,
     * 49152, 65536, 98304, 131072, 196608, 262144, 393216, 524288, 786432, 1048576, 1572864, 2097152,
     * 3145728, 4194304, 6291456, 8388608, 12582912, 16777216, 25165824, 33554432, 50331648, 67108864,
     * 100663296, 134217728, 201326592, 268435456, 402653184, 536870912, 805306368, 1073741824, 1610612736
     *
     *      LEN = 22 + 4
     * 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608,
     * 16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824, 2147483648
     */
public:
    Bin() = default;

    Bin(void *pool, int memSize);

    void *allocate(int size);

    bool free(void *blk);

    Block *distributeMem(void *blockStart, int size, bool debug = false);

    void printAll();

    Head *bins;

    void *blockStarts = nullptr;

    void *memEnd = nullptr;

    int memSize = 0;

    int count = 0;
};

const int BIN_COUNT = 22 + 4;
Bin g_BIN;

int getBlockSize(int dataSize) {        // new
    int minimalSize = dataSize + sizeof(Block) + sizeof(int);   // 4B for int size at tail
    if (minimalSize <= 64) return 64;

    double power = ceil(log(minimalSize) / log(2));

    return pow(2, power);
}

int getBiggestBlockSize(int freeSize) {
        double power = log(freeSize) / log(2);
        int floored = floor(power);

        return pow(2, floored);
}

/**
 * Compute index to bin
 * @param blockSize
 * @return
 */
int getBinIdx(int blockSize) {  // new
    double power = ceil(log(blockSize) / log(2));

    return ((int) power - 6);
}

void addUsed(Head *h, Block *b_ptr) {
    b_ptr->inuse = true;
    if (h->used_head == nullptr) {
        h->used_first = h->used_head = b_ptr;
        b_ptr->next_blok = b_ptr->prev_blok = nullptr;
        return;
    }
    h->used_head->next_blok = b_ptr;
    b_ptr->prev_blok = h->used_head;
    b_ptr->next_blok = nullptr;
    h->used_head = b_ptr;
}

void addFree(Head *h, Block *b_ptr) {
    b_ptr->inuse = false;
    if (h->free_head == nullptr) {
        h->free_first = h->free_head = b_ptr;
        b_ptr->next_blok = b_ptr->prev_blok = nullptr;
        return;
    }
    h->free_head->next_blok = b_ptr;
    b_ptr->prev_blok = h->free_head;
    b_ptr->next_blok = nullptr;
    h->free_head = b_ptr;
}

Block *removeLeastFree(Head *h) {
    if (h->free_head == nullptr) return nullptr;

    if (h->free_head == h->free_first) {
        Block *tmp = h->free_head;
        h->free_first = h->free_head = nullptr;
        return tmp;
    }

    Block *tmp = h->free_first;
    h->free_first = h->free_first->next_blok;
    return tmp;
}

Head *removeUsed(Block *block) {
    Head *h = &g_BIN.bins[getBinIdx(block->size)];

    if (h->used_first == h->used_head) {
        h->used_first = h->used_head = nullptr;
    } else if (h->used_first == block) {
        h->used_first = block->next_blok;
    } else if (h->used_head == block) {
        h->used_head = block->prev_blok;
    } else {
        Block *prev = block->prev_blok;
        prev->next_blok = block->next_blok;
        Block *next = block->next_blok;
        next->prev_blok = prev;
    }

    block->inuse = false;
    block->next_blok = nullptr;
    block->prev_blok = nullptr;

    return h;
}

Head *removeFree(Block *block) {
    Head *h = &g_BIN.bins[getBinIdx(block->size)];

    if (h->free_first == h->free_head) {
        h->free_first = h->free_head = nullptr;
    } else if (h->free_first == block) {
        h->free_first = block->next_blok;
    } else if (h->free_head == block) {
        h->free_head = block->prev_blok;
    } else {
        Block *prev = block->prev_blok;
        prev->next_blok = block->next_blok;
        Block *next = block->next_blok;
        next->prev_blok = prev;
    }

    block->next_blok = nullptr;
    block->prev_blok = nullptr;

    return h;
}

Block *getPrevChunk(Block *bl) {
    if (bl == g_BIN.blockStarts) return nullptr;

    int *prev_size = (int *) ((char *) bl - sizeof(int));
    return (Block *) ((char *) bl - *prev_size);
}

Block *getNextChunk(Block *bl) {
    if ((char *) bl + bl->size == g_BIN.memEnd) return nullptr;

    return (Block *) ((char *) bl + bl->size);
}

void removeUsedAndSpread(Block *block) {
    removeUsed(block);

    bool state = true;
    while(state) {
        Block *prev_chunk = getPrevChunk(block);
        Block *next_chunk = getNextChunk(block);
        state = false;

        if (prev_chunk && !prev_chunk->inuse && prev_chunk->size == block->size) {
            removeFree(prev_chunk);
            block = prev_chunk;
            *block = Block(block->size * 2, prev_chunk);
            state = true;
        } else if (next_chunk && !next_chunk->inuse && next_chunk->size == block->size) {
            removeFree(next_chunk);
            *block = Block(block->size * 2, block);
            state = true;
        }
    }

    addFree(&g_BIN.bins[getBinIdx(block->size)], block);
}

Bin::Bin(void *pool, int memorySize) {
    bins = static_cast<Head *>(pool);

    for (int i = 0; i < BIN_COUNT; ++i) {
        bins[i] = Head();
    }

    this->memSize = memorySize - BIN_COUNT * sizeof(Head);
    printf("free space: %d\n", this->memSize);

    blockStarts = ((char *) pool + sizeof(Head) * BIN_COUNT);
    this->memEnd = this->distributeMem(blockStarts, this->memSize, true);
}

int checkAddr(Block * block){
    return (void *) block == block->data - sizeof(Block) ? 0: 1;
}

void Bin::printAll() {
    printf("----------------\n");
    for (int i = 0; i < BIN_COUNT; ++i) {
        Block *current = bins[i].free_first;

        if (current) {
            printf("BIN %d %dB >", (int) pow(2, i +6), current->size);
            while (current) {
                printf(" - inuse=0,%d", checkAddr(current));
                current = current->next_blok;
            }
            printf("\n");
        }

        current = bins[i].used_first;

        if (current) {
            printf("BIN %d %dB >", (int) pow(2, i +6), current->size);
            while (current) {
                printf(" - inuse=1,%d", checkAddr(current));
                current = current->next_blok;
            }
            printf("\n");
        }
    }
    printf("^^^^^^^^^^^^^^\n");
}

void *Bin::allocate(int size) {
    int blockSize = getBlockSize(size);
    int idx = getBinIdx(blockSize);

    Block *current = nullptr;
    Head *h;
    while (idx < BIN_COUNT) {
        h = &this->bins[idx];
        current = removeLeastFree(h);

        if (current)
            break;

        ++idx;
    }
    if (current) {
        while (true) {
            if (current->size == blockSize) {
                addUsed(&this->bins[getBinIdx(current->size)],
                        current);
                ++this->count;
                return current->data;
            }

                int newSize = current->size / 2;
                *current = Block(newSize, current);
                addFree(&this->bins[getBinIdx(current->size)], current);

                current = (Block *) ((char *)current + newSize);
                *current = Block(newSize, current);
        }
    }


    return nullptr;
}

bool Bin::free(void *blk) {
    if ((char *) blk - sizeof(Block) >= blockStarts && blk < (char *) blockStarts + memSize) {

        auto *block = (Block *) ((char *) blk - sizeof(Block));
        if (!block->inuse || (char *) block + block->size > (char *) blockStarts + memSize)
            return false;

        if (block->size != *getSizeTag(block, block->size))
            return false;

        removeUsedAndSpread(block);
        --this->count;
        return true;
    }

    return false;
}

Block *Bin::distributeMem(void *blockStart, int size, bool debug) {
    printf("distributing %d\n", size);
    Block *block_addr = nullptr;
    while (size >= 64) {
        int blockSize = getBiggestBlockSize(size);

        block_addr = (Block *) blockStart;
        *block_addr = Block(blockSize, block_addr);

        addFree(&bins[getBinIdx(blockSize)], block_addr);

        printf("as %d\n", blockSize);

        blockStart = (char *) blockStart + blockSize;
        size = size - blockSize;
    }

    if (size > 0) printf("WARNING unused memory> %d\n", size);
    return (Block *) blockStart;
}

void HeapInit(void *memPool, int memSize) {

    g_BIN = Bin(memPool, memSize);

}

void *HeapAlloc(int size) {
    return g_BIN.allocate(size);
}

bool HeapFree(void *blk) {
    /* todo */

    return g_BIN.free(blk);
}

void HeapDone(int *pendingBlk) {
    *pendingBlk = g_BIN.count;
}

#ifndef __PROGTEST__

int main(void) {

    assert(getBinIdx(512) == 3);
    assert(getBinIdx(1024) == 4);
    assert(getBinIdx(2048) == 5);
    assert(getBlockSize(1048) == 2048);
    assert(getBlockSize(2) == 64);
    assert(getBlockSize(48) == 128);   // 36 is not divided by 8
    assert(getBiggestBlockSize(512) == 512);
    assert(getBiggestBlockSize(48) == 32);
    assert(getBiggestBlockSize(1025) == 1024);
    assert(getBiggestBlockSize(4500) == 4096);

    //printf("sizeof(Blok1) %d, sizeof(Block2) %d\n", sizeof(Block1), sizeof(Block2));
    static uint8_t memPool2[4 * 1024 - 296];
    HeapInit(memPool2, 4 * 1024 - 296);

    g_BIN.printAll();
    assert(HeapAlloc(2));
    g_BIN.printAll();
    assert(HeapAlloc(2));
    g_BIN.printAll();
    assert(HeapAlloc(2));
    g_BIN.printAll();
    assert(HeapAlloc(2));
    assert(HeapAlloc(2));
    assert(HeapAlloc(2));
    assert(HeapAlloc(2));
    assert(HeapAlloc(2));
    assert(HeapAlloc(2));
    assert(HeapAlloc(2));
    // g_BIN.printAll();
    assert(HeapAlloc(2));
    assert(HeapAlloc(112));
    assert(HeapAlloc(472));
    assert(HeapAlloc(800));
    assert(HeapAlloc(80));
    assert(HeapAlloc(180));
    assert(HeapAlloc(3));
    g_BIN.printAll();
    assert(HeapAlloc(2) == nullptr);

    static uint8_t memPool5[4 * 1024 - 296];
    void * start = memPool5;
    void * end = memPool5 + 4 * 1024 - 296;
    HeapInit(memPool5, 4 * 1024 - 296);
    g_BIN.printAll();
    for (int i = 0; i < 10; i++) {
        void *ptr;
        assert(ptr = HeapAlloc(2));
        g_BIN.printAll();
        assert(HeapFree(ptr));
    }

    g_BIN.printAll();


    static uint8_t memPool3[10 * 1024 + 1300];
    HeapInit(memPool3, 10 * 1024 + 1300);

    assert(HeapAlloc(800));
    g_BIN.printAll();
    assert(HeapAlloc(800));
    assert(HeapAlloc(800));
    assert(HeapAlloc(800));
    assert(HeapAlloc(800));
    assert(HeapAlloc(800));
    assert(HeapAlloc(800));
    g_BIN.printAll();
    assert(HeapAlloc(430));
    g_BIN.printAll();
    assert(HeapAlloc(220));
    assert(HeapAlloc(2));
    assert(HeapAlloc(2));
    assert(HeapAlloc(2));
    g_BIN.printAll();
    // assert(HeapAlloc(490) == nullptr);

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
    g_BIN.printAll();
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

