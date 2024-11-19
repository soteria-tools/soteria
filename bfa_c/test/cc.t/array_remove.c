void* malloc(int);
void free(void*);
void memcpy(void*, void*, int);
void ___bfa_debug_show();


typedef __cerbty_size_t size_t;

#define NULL (void*) 0

#define EXP_FACTOR 2
#define DEFAULT_CAP 8

#define CC_MAX_ELEMENTS 0x7FFFFFFF
#define CC_OK 0
#define CC_ERR_ALLOC 1
#define CC_ERR_MAX_CAPACITY 2


struct array_s {
    size_t   size;
    size_t   capacity;
    void   **buffer;

    // void *(*mem_alloc)  (size_t size);
    // void *(*mem_calloc) (size_t blocks, size_t size);
    // void  (*mem_free)   (void *block);
};

typedef struct array_s Array;

int array_new(Array **out) {
  Array *ar = malloc(sizeof(Array));
  if (!ar) return 1;
  void** buff = malloc(DEFAULT_CAP * sizeof(void*));
  if (!buff) {
    free(ar);
    return 1;
  }
  ar->buffer = buff;
  ar->capacity = DEFAULT_CAP;
  ar->size = 0;
  *out = ar;
  return 0;
}

int expand_capacity(Array *ar) {
    if (ar->capacity == CC_MAX_ELEMENTS)
        return CC_ERR_MAX_CAPACITY;

    size_t new_capacity = ar->capacity * EXP_FACTOR;

    /* As long as the capacity is greater that the expansion factor
     * at the point of overflow, this is check is valid. */
    if (new_capacity <= ar->capacity)
        ar->capacity = CC_MAX_ELEMENTS;
    else
        ar->capacity = new_capacity;

    void **new_buff = malloc(ar->capacity * sizeof(void *));

    if (!new_buff)
        return CC_ERR_ALLOC;

    memcpy(new_buff, ar->buffer, ar->size * sizeof(void *));
    free(ar->buffer);
    ar->buffer = new_buff;

    return CC_OK;
}

int array_add(Array *ar, void* element) {
  if (ar->size >= ar->capacity) {
    int stat = expand_capacity(ar);
    ___bfa_debug_show ();
    if (stat != 0) return stat;
  }
  ar->buffer[ar->size] = element;
  ar->size++;
  return CC_OK;
}

// TODO: This is an excellent example to experiment with tree rebalancing.
// When increasing the loop limit (currently 50), things get extremely slow.
// I suspect it the time to access the correct cell in the heap that just gets deeper and deeper into the corresponding tree.

int main() {
  Array *v1 = malloc(sizeof(Array));
  if (!v1) return CC_ERR_ALLOC;
  int stat = array_new(&v1);
  for (int i = 0;  i < 50; i++) {
    if (stat != 0) return stat;
    stat = array_add(v1, NULL);
  }
  return 0;
}