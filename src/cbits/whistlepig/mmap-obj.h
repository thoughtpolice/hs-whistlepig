#ifndef WP_MMAP_OBJ_H_
#define WP_MMAP_OBJ_H_

// whistlepig mmap objects
// (c) 2011 William Morgan. See COPYING for license terms.
//
// wrappers around the logic of loading, unloading, and resizing
// arbitrary-sized objects using mmap.
//
// note that any of the mmap_obj_* functions may change the object pointer, so
// use MMAP_OBJ or MAP_OBJ_PTR to dereference (again) after calling them.

#define MMAP_OBJ_MAGIC_SIZE 15

#include <stdint.h>
#include "error.h"

// what's actually mmap'd
typedef struct mmap_obj_header {
  char magic[MMAP_OBJ_MAGIC_SIZE];
  uint32_t size; // size of payload, not including this header
  char obj[];    // the payload itself
} mmap_obj_header;

// what we pass around at runtime
typedef struct mmap_obj {
  int fd;
  uint32_t loaded_size; // compare against header->sizer
  mmap_obj_header* content;
} mmap_obj;

// public API

// public: get the actual object from an mmap_obj
#define MMAP_OBJ(v, type) ((type*)&v.content->obj)

// public: get the object from an mmap_obj*
#define MMAP_OBJ_PTR(v, type) (type*)v->content->obj

// public: create an object with an initial size
wp_error* mmap_obj_create(mmap_obj* o, const char* magic, const char* pathname, uint32_t initial_size) RAISES_ERROR;

// public: load an object, raising an error if it doesn't exist (or if the
// magic doesn't match)
wp_error* mmap_obj_load(mmap_obj* o, const char* magic, const char* pathname) RAISES_ERROR;

// public: load an object, but only if the size has changed since the
// first load.
wp_error* mmap_obj_reload(mmap_obj* o) RAISES_ERROR;

// public: resize an object. note that the obj pointer might change after this call.
wp_error* mmap_obj_resize(mmap_obj* o, uint32_t new_size) RAISES_ERROR;

// public: unload an object
wp_error* mmap_obj_unload(mmap_obj* o) RAISES_ERROR;

#endif
