#ifndef SCM_STACKS_H
#define SCM_STACKS_H

/* Copyright 1995-1996,2000-2001,2004,2006,2008,2018
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */



#include <libguile/error.h>
#include "libguile/frames.h"

/* {Frames and stacks}
 */

SCM_API SCM scm_stack_type;

#define SCM_STACK_LAYOUT                        \
  "pw" /* len */                                \
  "pw" /* id */                                 \
  "pw" /* frame */

#define SCM_STACKP(obj) (SCM_STRUCTP (obj) && scm_is_eq (SCM_STRUCT_VTABLE (obj), scm_stack_type))
#define SCM_STACK_LENGTH(obj) (scm_to_long (SCM_STRUCT_SLOT_REF (obj,0)))
#define SCM_SET_STACK_LENGTH(obj,f) (SCM_STRUCT_SLOT_SET (obj,0,scm_from_long (f)))
#define SCM_STACK_ID(obj) (SCM_STRUCT_SLOT_REF (obj,1))
#define SCM_SET_STACK_ID(obj,f) (SCM_STRUCT_SLOT_SET (obj,1,f))
#define SCM_STACK_FRAME(obj) (SCM_STRUCT_SLOT_REF (obj,2))
#define SCM_SET_STACK_FRAME(obj,f) (SCM_STRUCT_SLOT_SET (obj,2,f))

#define SCM_FRAMEP(obj) (SCM_VM_FRAME_P (obj))

#define SCM_VALIDATE_STACK(pos, v) \
  SCM_MAKE_VALIDATE_MSG (pos, v, STACKP, "stack")
#define SCM_VALIDATE_FRAME(pos, v) \
  SCM_MAKE_VALIDATE_MSG (pos, v, FRAMEP, "frame")




SCM_API SCM scm_stack_p (SCM obj);
SCM_API SCM scm_make_stack (SCM obj, SCM args);
SCM_API SCM scm_stack_id (SCM stack);
SCM_API SCM scm_stack_ref (SCM stack, SCM i);
SCM_API SCM scm_stack_length (SCM stack);

SCM_INTERNAL void scm_init_stacks (void);

#endif  /* SCM_STACKS_H */
