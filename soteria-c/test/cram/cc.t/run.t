  $ soteria-c exec array_add.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  error: Buffer overflow or underflow in main
      ┌─ array_add.c:104:5
  104 │      ar->buffer[ar->size] = element;
      │      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Triggering write
  105 │      ar->size++;
  106 │      return CC_OK;
  107 │  }
  108 │  
  109 │  int array_index_of(Array *ar, void *element, size_t *index)
  110 │  {
  111 │      size_t i;
  112 │      for (i = 0; i < ar->size; i++)
  113 │      {
  114 │          if (ar->buffer[i] == element)
  115 │          {
  116 │              *index = i;
  117 │              return CC_OK;
  118 │          }
  119 │      }
  120 │      return CC_ERR_OUT_OF_RANGE;
  121 │  }
  122 │  
  123 │  int array_remove(Array *ar, void *element, void **out)
  124 │  {
  125 │      size_t index;
  126 │      int status = array_index_of(ar, element, &index);
  127 │  
  128 │      if (status == CC_ERR_OUT_OF_RANGE)
  129 │          return CC_ERR_VALUE_NOT_FOUND;
  130 │  
  131 │      if (index != ar->size - 1)
  132 │      {
  133 │          size_t block_size = (ar->size - index) * sizeof(void *);
  134 │  
  135 │          memcpy(&(ar->buffer[index]), &(ar->buffer[index + 1]), block_size);
  136 │      }
  137 │      ar->size--;
  138 │  
  139 │      if (out)
  140 │          *out = element;
  141 │  
  142 │      return CC_OK;
  143 │  }
  144 │  
  145 │  int main()
  146 │  {
  147 │      Array *v1 = malloc(sizeof(Array));
  148 │      if (!v1)
  149 │          return CC_ERR_ALLOC;
  150 │  
  151 │      int stat = array_new(&v1);
  152 │      if (stat != 0)
  153 │          return stat;
  154 │  
  155 │      for (int i = 0; i < 10; i++)
  156 │      {
  157 │          array_add(v1, NULL);
      │          -------------------- 1: Called from here
  Executed 163 statements
  Verification Failure!
