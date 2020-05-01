#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "memory.h"
#include "fail.h"
#include "engine.h"

#if GC_VERSION == GC_MARK_N_SWEEP

static void* memory_start = NULL;
static void* memory_end = NULL;

static uvalue_t* bitmap_start = NULL;

static value_t* heap_start = NULL;
static value_t* heap_end = NULL;
static value_t heap_start_v = 0;
static value_t heap_end_v = 0;
static value_t* heap_first_block = NULL;

#define FREE_LISTS_COUNT 32
static value_t* free_list_heads[FREE_LISTS_COUNT];

#define MIN_BLOCK_SIZE 1
#define HEADER_SIZE 1

// Header management

static value_t header_pack(tag_t tag, value_t size) {
  return (size << 8) | (value_t)tag;
}

static tag_t header_unpack_tag(value_t header) {
  return (tag_t)(header & 0xFF);
}

static value_t header_unpack_size(value_t header) {
  return header >> 8;
}

// Bitmap management

static int bitmap_is_bit_set(value_t* ptr) {
  assert(heap_start <= ptr && ptr < heap_end);
  long index = ptr - heap_start;
  long word_index = index / (long)VALUE_BITS;
  long bit_index = index % (long)VALUE_BITS;
  return (bitmap_start[word_index] & ((uvalue_t)1 << bit_index)) != 0;
}

static void bitmap_set_bit(value_t* ptr) {
  assert(heap_start <= ptr && ptr < heap_end);
  long index = ptr - heap_start;
  long word_index = index / (long)VALUE_BITS;
  long bit_index = index % (long)VALUE_BITS;
  bitmap_start[word_index] |= (uvalue_t)1 << bit_index;
}

static void bitmap_clear_bit(value_t* ptr) {
  assert(heap_start <= ptr && ptr < heap_end);
  long index = ptr - heap_start;
  long word_index = index / (long)VALUE_BITS;
  long bit_index = index % (long)VALUE_BITS;
  bitmap_start[word_index] &= ~((uvalue_t)1 << bit_index);
}

// Virtual <-> physical address translation

static void* addr_v_to_p(value_t v_addr) {
  return (char*)memory_start + v_addr;
}

static value_t addr_p_to_v(void* p_addr) {
  return (value_t)((char*)p_addr - (char*)memory_start);
}

// Free lists management

static value_t real_size(value_t size) {
  assert(0 <= size);
  return size < MIN_BLOCK_SIZE ? MIN_BLOCK_SIZE : size;
}

static unsigned int free_list_index(value_t size) {
  assert(0 <= size);
  return size >= FREE_LISTS_COUNT ? FREE_LISTS_COUNT - 1 : (unsigned int)size;
}

char* memory_get_identity() {
  return "mark & sweep garbage collector";
}

void memory_setup(size_t total_byte_size) {
  memory_start = malloc(total_byte_size);
  if (memory_start == NULL)
    fail("cannot allocate %zd bytes of memory", total_byte_size);
  memory_end = (char*)memory_start + total_byte_size;
}

void memory_cleanup() {
  assert(memory_start != NULL);
  free(memory_start);

  memory_start = memory_end = NULL;
  bitmap_start = NULL;
  heap_start = heap_end = NULL;
  heap_start_v = heap_end_v = 0;
  for (int l = 0; l < FREE_LISTS_COUNT; ++l)
    free_list_heads[l] = NULL;
}

void* memory_get_start() {
  return memory_start;
}

void* memory_get_end() {
  return memory_end;
}

void memory_set_heap_start(void* ptr) {
  assert(memory_start <= ptr && ptr < memory_end);

  const size_t bh_size =
    (size_t)((char*)memory_end - (char*)ptr) / sizeof(value_t);

  const size_t bitmap_size = (bh_size - 1) / (VALUE_BITS + 1) + 1;
  const size_t heap_size = bh_size - bitmap_size;

  bitmap_start = ptr;
  memset(bitmap_start, 0, bitmap_size * sizeof(value_t));

  heap_start = (value_t*)bitmap_start + bitmap_size;
  heap_end = heap_start + heap_size;
  assert(heap_end == memory_end);

  heap_start_v = addr_p_to_v(heap_start);
  heap_end_v = addr_p_to_v(heap_end);

  heap_first_block = heap_start + HEADER_SIZE;
  const value_t initial_block_size = (value_t)(heap_end - heap_first_block);
  heap_first_block[-1] = header_pack(tag_None, initial_block_size);
  heap_first_block[0] = 0;

  for (int l = 0; l < FREE_LISTS_COUNT - 1; ++l)
    free_list_heads[l] = memory_start;
  free_list_heads[FREE_LISTS_COUNT - 1] = heap_first_block;
}

static value_t* allocate(tag_t tag, value_t size) {
  printf("1: %d\n", tag);
  assert(0 <= size);
  printf("Size is greater!\n");
  value_t * nul = addr_v_to_p(0); //making null
  
  int FreeListC = FREE_LISTS_COUNT - 1;

  value_t * prev = nul;
  value_t * curr = free_list_heads[FreeListC];

  value_t asize = real_size(size);
  printf("asize: %d\n", asize);

  /* make sure curr is not nul and memory block size is greater then the real size */
  while ((curr != nul) && (memory_get_block_size(curr) < asize)) {
    prev = curr; // make prev the curr
    curr = addr_v_to_p(curr[0]); // move the curr
  }

  if (curr == nul) return NULL; // end
  
  value_t * curr_next = addr_v_to_p(curr[0]); // move curr

  value_t curr_size = memory_get_block_size(curr); // get size
  
  int minSize = curr_size - asize - HEADER_SIZE;
  if (minSize < MIN_BLOCK_SIZE) { // curr size is less then minblock
    prev == nul ? (free_list_heads[FreeListC] = curr_next) : (prev[0] = addr_p_to_v(curr_next));
    curr[-1] = header_pack(tag, curr_size);
  } else { // curr size is greater then minblock
    curr[-1] = header_pack(tag, asize);
    value_t * next = curr + asize + HEADER_SIZE;
    next[-1] = header_pack(tag_None, minSize);
    next[0] = addr_p_to_v(curr_next);
    prev == nul ? (free_list_heads[FreeListC] = next) : (prev[0] = addr_p_to_v(next));
  }
  bitmap_set_bit(curr);
  return curr;
}

static void mark(value_t* block) {
  if((addr_p_to_v(block) & 3) != 0) return;

  if(!bitmap_is_bit_set(block)) return;
  
  bitmap_clear_bit(block);

  for(int i = 0; i < memory_get_block_size(block); i++) {
    if((block[i] >= heap_start_v) && (block[i] < heap_end_v)) {
      mark(addr_v_to_p(block[i]));
    }
  }
}

static void sweep() {
  value_t *nul = addr_v_to_p(0);

  value_t *curr = heap_first_block;
  value_t *prev = nul;

  while (curr < heap_end) {

    if (!bitmap_is_bit_set(curr) && memory_get_block_tag(curr) != tag_None) { // not tag_none
      bitmap_set_bit(curr); // clear
    } else { // is Tag_none
      bitmap_clear_bit(curr);
      value_t currentSize = memory_get_block_size(curr);
      curr[-1] = header_pack(tag_None, currentSize);
      if(prev == nul) {
        free_list_heads[FREE_LISTS_COUNT - 1] = curr;
        prev = curr;
      }

      if ((prev != nul) && ((prev + memory_get_block_size(prev) + HEADER_SIZE) == curr)) {
        value_t prev_size = memory_get_block_size(prev);
        prev[-1] = header_pack(tag_None, prev_size + currentSize + HEADER_SIZE);
      } else {
        prev[0] = addr_p_to_v(curr);
        prev = curr;
      }
    }
    curr = curr + memory_get_block_size(curr) + HEADER_SIZE;
  }
  prev[0] = 0;
}

value_t* memory_allocate(tag_t tag, value_t size) {
  value_t* first_try = allocate(tag, size);
  if (first_try != NULL)
    return first_try;

  value_t* lb = engine_get_Lb();
  if (lb != memory_start) mark(lb);
  value_t* ib = engine_get_Ib();
  if (ib != memory_start) mark(ib);
  value_t* ob = engine_get_Ob();
  if (ob != memory_start) mark(ob);

  sweep();

  value_t* second_try = allocate(tag, size);
  if (second_try != NULL)
    return second_try;

  fail("\ncannot allocate %d words of memory, even after GC\n", size);
}

value_t memory_get_block_size(value_t* block) {
  return header_unpack_size(block[-1]);
}

tag_t memory_get_block_tag(value_t* block) {
  return header_unpack_tag(block[-1]);
}

#endif
