(*********************************************************************************)
(*                Cameleon                                                       *)
(*                                                                               *)
(*    Copyright (C) 2004-2010 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Library General Public License as            *)
(*    published by the Free Software Foundation; either version 2 of the         *)
(*    License, or any later version.                                             *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Library General Public          *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

val prefix : string
val bindir : string
val libdir : string
val manext : string
val ranlib : string
val ranlibcmd : string
val sharpbangscripts : bool
val bng_arch : string
val bng_asm_level : string
val pthread_link : string
val x11_includes : string
val x11_link : string
val tk_link : string
val dbm_includes : string
val dbm_link : string
val bytecc : string
val bytecccompopts : string
val bytecclinkopts : string
val bytecclibs : string
val byteccrpath : string
val exe : string
val supports_shared_libraries : bool
val sharedcccompopts : string
val mksharedlibrpath : string
val arch : string
val model : string
val system : string
val nativecc : string
val nativecccompopts : string
val nativeccprofopts : string
val nativecclinkopts : string
val nativeccrpath : string
val nativecclibs : string
val packld : string
val dllcccompopts : string
val asm : string
val aspp : string
val asppprofflags : string
val profiling : string
val dynlinkopts : string
val otherlibraries : string
val debugger : string
val cc_profile : string
val systhread_support : bool
val syslib : string -> string
val mkexe : string
val mkdll : string
val mkmaindll : string
val mklib : string -> string -> string -> string
val ext_lib : string
val ext_obj : string
val ext_asm : string
val ext_dll : string
val o : string
val a : string
val so : string
val toolchain : string
val ccomptype : string
val extralibs : string
val tk_defs : string
