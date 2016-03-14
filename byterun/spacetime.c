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

#include <assert.h>
#include "caml/fail.h"
#include "caml/mlvalues.h"

int ensure_spacetime_dot_o_is_included = 42;

CAMLprim value caml_spacetime_only_works_for_native_code(value foo, ...)
{
  caml_failwith("Spacetime profiling only works for native code");
  assert(0);  /* unreachable */
}

uintnat caml_spacetime_my_profinfo (void)
{
  return 0;
}
