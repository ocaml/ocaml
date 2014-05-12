/***********************************************************************/
/*                                                                     */
/*                               OCaml                                 */
/*                                                                     */
/*                 Mark Shinwell, Jane Street Europe                   */
/*                                                                     */
/*  Copyright 2013, Jane Street Holding                                */
/*                                                                     */
/*  Licensed under the Apache License, Version 2.0 (the "License");    */
/*  you may not use this file except in compliance with the License.   */
/*  You may obtain a copy of the License at                            */
/*                                                                     */
/*      http://www.apache.org/licenses/LICENSE-2.0                     */
/*                                                                     */
/*  Unless required by applicable law or agreed to in writing,         */
/*  software distributed under the License is distributed on an        */
/*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       */
/*  either express or implied.  See the License for the specific       */
/*  language governing permissions and limitations under the License.  */
/*                                                                     */
/***********************************************************************/

/* Runtime support for allocation profiling. */

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#ifndef __APPLE__
#include <elf.h>
#endif
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>

#include "alloc.h"
#include "gc.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "mlvalues.h"
#include "signals.h"

int ensure_alloc_profiling_dot_o_is_included = 42;

/* Determine the byte offset of a given section in an ELF file. */
CAMLprim value
caml_byte_offset_of_source_location_map_elf_section_contents(value v_executable,
                                                             value v_section_name)
{
#ifndef __APPLE__
  int fd;
  int bytes_read;
  char* filename;
  char* section_name;
  Elf64_Ehdr elf_header;
  Elf64_Half section_header_index;
  off_t section_header_string_table_file_offset;
  off_t file_offset;
  Elf64_Shdr section_header;
  off_t found_section_at_offset = -1;

  assert(Is_block(v_executable) && Tag_val(v_executable) == String_tag);
  assert(Is_block(v_section_name) && Tag_val(v_section_name) == String_tag);

  filename = strdup(String_val(v_executable));
  section_name = strdup(String_val(v_section_name));

  if (filename == NULL || section_name == NULL) {
    return Val_long(-1);
  }

  caml_enter_blocking_section();

  fd = open(filename, O_RDONLY);
  free(filename);
  if (fd < 0) {
    free(section_name);
    caml_leave_blocking_section();
    return Val_long(-1);
  }

  /* Read the ELF file header. */
  bytes_read = read(fd, &elf_header, sizeof(elf_header));
  /* CR mshinwell: extend to support 32-bit executables */
  if (bytes_read < sizeof(elf_header)
      || elf_header.e_ident[EI_CLASS] != ELFCLASS64
      || elf_header.e_shentsize != sizeof(Elf64_Shdr)) {
    free(section_name);
    (void) close(fd);
    caml_leave_blocking_section();
    return Val_long(-1);
  }

  /* Read the section header for the section header string table. */
  file_offset = elf_header.e_shoff + sizeof(section_header)*elf_header.e_shstrndx;
  if (lseek(fd, file_offset, SEEK_SET) < 0
      || read(fd, &section_header, sizeof(section_header)) < sizeof(section_header)) {
    free(section_name);
    (void) close(fd);
    caml_leave_blocking_section();
    return Val_long(-1);
  }
  section_header_string_table_file_offset = section_header.sh_offset;

  /* Iterate over each section looking for the desired section by name.  The name of
     each section is determined by reading a piece of the section header string table. */
  for (section_header_index = 0;
       found_section_at_offset == -1 && section_header_index < elf_header.e_shnum;
       section_header_index++) {
    char* desired_section_name;
    int name_of_this_section_does_not_match;

    if (lseek(fd, elf_header.e_shoff + sizeof(section_header)*section_header_index,
              SEEK_SET) < 0
        || read(fd, &section_header, sizeof(section_header)) < sizeof(section_header)
        || lseek(fd, section_header_string_table_file_offset + section_header.sh_name,
                 SEEK_SET) < 0) {
      free(section_name);
      (void) close(fd);
      caml_leave_blocking_section();
      return Val_long(-1);
    }

    desired_section_name = section_name;
    name_of_this_section_does_not_match = 0;
    while (!name_of_this_section_does_not_match && *desired_section_name) {
      char ch;
      if (read(fd, &ch, 1) < 1) {
        free(section_name);
        (void) close(fd);
        caml_leave_blocking_section();
        return Val_long(-1);
      }
      if (ch != *desired_section_name++) {
        name_of_this_section_does_not_match = 1;
      }
    }

    if (!name_of_this_section_does_not_match) {
      found_section_at_offset = section_header.sh_offset;
    }
  }

  free(section_name);
  (void) close(fd);
  caml_leave_blocking_section();

  return Val_long(found_section_at_offset);
#else
  /* Mac OS X does not use ELF. */
  v_executable = v_executable;
  v_section_name = v_section_name;
  return Val_long(-1);
#endif
}

void
caml_dump_allocators_of_major_heap_blocks (const char* output_file)
{
  char* chunk;
  FILE* fp;
  uint64_t unaccounted_for = 0ull;

  fp = fopen(output_file, "w");
  if (fp == NULL) {
    fprintf(stderr, "couldn't open file '%s' for heap block dump\n", output_file);
    return;
  }

  /* To avoid having to traverse the minor heap, just empty it. */
  caml_minor_collection();

  /* Perform a full major collection so no white blocks remain. */
  caml_finish_major_cycle();

  chunk = caml_heap_start;

  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd = Hd_hp (hp);
      switch (Color_hd(hd)) {
        case Caml_blue:
          break;

        default: {
          uint64_t approx_instr_pointer;

          approx_instr_pointer = Decode_profinfo_hd(hd);
          if (approx_instr_pointer != 0ull) {
            uint64_t size_in_words_including_header;
            const char* colour;

            size_in_words_including_header = Whsize_hd(hd);
            /* CR mshinwell: after recent changes, we'll only have white here */
            switch (Color_hd(hd)) {
              case Caml_black: colour = "b"; break;
              case Caml_gray: colour = "g"; break;
              case Caml_white: colour = "w"; break;
              default: assert(0);
            }

            fprintf(fp, "%p %lld %s\n", (void*) approx_instr_pointer,
                    (unsigned long long) size_in_words_including_header, colour);
          }
          else {
            unaccounted_for++;
          }
          break;
        }
      }
      hp += Bhsize_hd (hd);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  fprintf(fp, "blocks unaccounted for: %lld\n", (unsigned long long) unaccounted_for);

  fclose(fp);
}

CAMLprim value
caml_dump_allocators_of_major_heap_blocks_from_ocaml (value output_file)
{
  assert(Is_block(output_file) && Tag_val(output_file) == String_tag);
  caml_dump_allocators_of_major_heap_blocks(String_val(output_file));
  return Val_unit;
}

CAMLprim value
caml_where_was_this_allocated (value v)
{
  uint64_t approx_instr_pointer;
  CAMLparam0();
  CAMLlocal1(v_approx_instr_pointer);
  value v_result;

  if (!Is_block(v)) {
    CAMLreturn(Val_long(0));  /* None */
  }

  approx_instr_pointer = Decode_profinfo_hd (Hd_val (v));

  if (approx_instr_pointer == 0ull) {
    CAMLreturn(Val_long(0));  /* None */
  }

  v_approx_instr_pointer = caml_copy_int64(approx_instr_pointer);

  v_result = caml_alloc_small(1, 0 /* Some */);
  Field(v_result, 0) = v_approx_instr_pointer;

  CAMLreturn(v_result);
}

CAMLprim value
caml_forget_where_values_were_allocated (value v_unit)
{
  char* chunk;

  assert(v_unit == Val_unit);

  caml_minor_collection();

  chunk = caml_heap_start;

  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd = Hd_hp (hp);
      Hd_hp (hp) = Make_header_with_profinfo (Wosize_hd(hd), Tag_hd(hd), Color_hd(hd), 0);
      hp += Bhsize_hd (hd);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  return v_unit;
}

void
caml_dump_heapgraph(const char* node_output_file, const char* edge_output_file)
{
  char* chunk;
  FILE* node_fp;
  FILE* edge_fp;

  node_fp = fopen(node_output_file, "w");
  if (node_fp == NULL) {
    fprintf(stderr, "couldn't open file '%s' for heap graph dump\n", node_output_file);
    return;
  }

  edge_fp = fopen(edge_output_file, "w");
  if (edge_fp == NULL) {
    fprintf(stderr, "couldn't open file '%s' for heap graph dump\n", edge_output_file);
    return;
  }

  caml_minor_collection();
  caml_finish_major_cycle();

  chunk = caml_heap_start;

  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd_parent = Hd_hp (hp);
      switch (Color_hd(hd_parent)) {
        case Caml_blue:
          break;

        default: {
          uint64_t approx_instr_pointer_parent;

          approx_instr_pointer_parent = Decode_profinfo_hd (hd_parent);

          if (approx_instr_pointer_parent != 0ull) {
            fprintf(node_fp, "%p\n", (void*) approx_instr_pointer_parent);

            if (Tag_hd(hd_parent) < No_scan_tag) {
              mlsize_t field;
              value parent;

              parent = Val_hp(hp);
              assert(Is_block(parent));

              for (field = 0; field < Wosize_val(parent); field++) {
                value child;
                child = Field(parent, field);

                if (Is_block(child) && Is_in_value_area(child)) {
                  uint64_t approx_instr_pointer_child;
                  header_t hd_child;

                  hd_child = Hd_val(child);
                  approx_instr_pointer_child = Decode_profinfo_hd (hd_child);

                  if (approx_instr_pointer_child != 0ull) {
                    fprintf(edge_fp, "%p,%p\n",
                            (void*) approx_instr_pointer_parent,
                            (void*) approx_instr_pointer_child);
                  }
                }
              }
            }
          }
          break;
        }
      }
      hp += Bhsize_hd (hd_parent);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  fclose(node_fp);
  fclose(edge_fp);
}

CAMLprim value
caml_dump_heapgraph_from_ocaml(value node_output_file, value edge_output_file)
{
  assert(Is_block(node_output_file) && Tag_val(node_output_file) == String_tag);
  assert(Is_block(edge_output_file) && Tag_val(edge_output_file) == String_tag);

  caml_dump_heapgraph(String_val(node_output_file), String_val(edge_output_file));

  return Val_unit;
}

#define MAX_LOG2_OBJECT_SIZE 9
typedef struct {
  uint64_t num_words_by_log2_object_size[MAX_LOG2_OBJECT_SIZE + 1];
  uint64_t num_blocks_by_log2_object_size[MAX_LOG2_OBJECT_SIZE + 1];
  uint64_t total_words;
  uint64_t total_blocks;
} lifetime_bucket;
static lifetime_bucket* lifetime_buckets_minor = NULL;
static lifetime_bucket* lifetime_buckets_major = NULL;
static double lifetime_log10_bytes_min = 2.0;
static double lifetime_log10_bytes_max = 8.0;
static uint64_t num_lifetime_buckets = 1000;
static double lifetime_bucket_width;
extern uintnat caml_lifetime_shift;

static void
init_lifetime_buckets(void)
{
  char* bytes_min_env;
  char* bytes_max_env;
  char* num_buckets_env;

  bytes_min_env = getenv("CAML_LIFETIME_MIN_BYTES");
  bytes_max_env = getenv("CAML_LIFETIME_MAX_BYTES");
  num_buckets_env = getenv("CAML_LIFETIME_NUM_BUCKETS");

  if (bytes_min_env) {
    lifetime_log10_bytes_min = log((double) atoll(bytes_min_env)) / log(10.0);
  }
  if (bytes_max_env) {
    lifetime_log10_bytes_max = log((double) atoll(bytes_max_env)) / log(10.0);
  }
  if (num_buckets_env) {
    num_lifetime_buckets = (uint64_t) atoll(num_buckets_env);
  }

  if (lifetime_log10_bytes_min > lifetime_log10_bytes_max) {
    fprintf(stderr, "maximum lifetime must be greater than minimum\n");
    abort();
  }

  lifetime_buckets_minor =
    (lifetime_bucket*) calloc(num_lifetime_buckets, sizeof(lifetime_bucket));
  lifetime_buckets_major =
    (lifetime_bucket*) calloc(num_lifetime_buckets, sizeof(lifetime_bucket));

  lifetime_bucket_width =
    (lifetime_log10_bytes_max - lifetime_log10_bytes_min) / num_lifetime_buckets;
}

void
caml_record_lifetime_sample(header_t hd, int in_major_heap, uint64_t now)
{
  uint64_t allocation_time;
  allocation_time = Decode_profinfo_hd(hd);

  if (allocation_time > 0) {  /* in case we failed to annotate a block */
    if (now >= allocation_time) {
      uint64_t lifetime;
      uint64_t bucket;
      double log10_lifetime;

      if (!lifetime_buckets_minor) {
        init_lifetime_buckets();
      }

      lifetime = now - allocation_time;
      log10_lifetime = log((double) (lifetime + 1)) / log(10.0);

      if (log10_lifetime >= lifetime_log10_bytes_min
            && log10_lifetime < lifetime_log10_bytes_max) {
        bucket = (log10_lifetime - lifetime_log10_bytes_min) / lifetime_bucket_width;

        if (bucket >= 0 && bucket < num_lifetime_buckets) {  /* just in case */
          lifetime_bucket* buckets;

          int log2_object_size = (int) (floor(log((double) (Wosize_hd(hd)))));
          if (log2_object_size > MAX_LOG2_OBJECT_SIZE) {
            log2_object_size = MAX_LOG2_OBJECT_SIZE;
          }

          buckets = in_major_heap ? lifetime_buckets_major : lifetime_buckets_minor;

          buckets[bucket].num_blocks_by_log2_object_size[log2_object_size]++;
          buckets[bucket].total_blocks++;

          buckets[bucket].num_words_by_log2_object_size[log2_object_size] += Wosize_hd(hd);
          buckets[bucket].total_words += Wosize_hd(hd);
        }
      }
    }
  }
}

void
caml_dump_lifetimes(void)
{
  uint64_t bucket;

  /* Output format (columns left to right):
        - centre of lifetime bucket, units are log10(bytes allocated)
        - total number of blocks in this lifetime bucket, minor heap
        - total number of words in this lifetime bucket, minor heap
        - total number of blocks in this lifetime bucket, major heap
        - total number of words in this lifetime bucket, major heap
        then a sequence of MAX_LOG2_OBJECT_SIZE+1 column sets, each as follows, giving
        object sizes in this lifetime bucket:
        - minimum number of words in a block in this size bucket
        - maximum number of words in a block in this size bucket
        - number of blocks in this size bucket, minor heap
        - number of words in this size bucket, minor heap
        - number of blocks in this size bucket, major heap
        - number of words in this size bucket, major heap
     Lines are not output for lifetime buckets that are empty.
  */

  for (bucket = 0ull; bucket < num_lifetime_buckets; bucket++) {
    int size_bucket;
    double centre_of_bucket =
      (lifetime_bucket_width * (double) bucket) + (lifetime_bucket_width / 2.0);

    if (lifetime_buckets_minor[bucket].total_blocks != 0ull
          || lifetime_buckets_major[bucket].total_words != 0ull) {
      fprintf(stderr, "%g %lld %lld %lld %lld",
        lifetime_log10_bytes_min + centre_of_bucket,
        (unsigned long long) lifetime_buckets_minor[bucket].total_blocks,
        (unsigned long long) lifetime_buckets_minor[bucket].total_words,
        (unsigned long long) lifetime_buckets_major[bucket].total_blocks,
        (unsigned long long) lifetime_buckets_major[bucket].total_words);
      for (size_bucket = 0; size_bucket <= MAX_LOG2_OBJECT_SIZE; size_bucket++) {
        fprintf(stderr, " %d %d %lld %lld %lld %lld",
          (int) (pow(2.0, size_bucket)),
          (int) (pow(2.0, size_bucket + 1) - 1),
          (unsigned long long)
            lifetime_buckets_minor[bucket].num_blocks_by_log2_object_size[size_bucket],
          (unsigned long long)
            lifetime_buckets_minor[bucket].num_words_by_log2_object_size[size_bucket],
          (unsigned long long)
            lifetime_buckets_major[bucket].num_blocks_by_log2_object_size[size_bucket],
          (unsigned long long)
            lifetime_buckets_major[bucket].num_words_by_log2_object_size[size_bucket]);
      }
      fprintf(stderr, "\n");
    }
  }
}
