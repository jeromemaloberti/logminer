(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(* Author: Jerome Maloberti <jerome.maloberti@citrix.com> *)

%%{
  machine date_time;

  action jan { month := 0 }
  action feb { month := 1 }
  action mar { month := 2 }
  action apr { month := 3 }
  action may { month := 4 }
  action jun { month := 5 }
  action jul { month := 6 }
  action aug { month := 7 }
  action sep { month := 8 }
  action oct { month := 9 }
  action nov { month := 10 }
  action dec { month := 11 }
  action day { day := !_int}
  action hour { hour := !_int}
  action min { min := !_int}
  action sec { sec := !_int}
  action clear_int { _int := 0 }
  action add_int { _int := !_int * 10 + (Char.code fc - Char.code '0')}

  month_re = ( 'Jan' @jan | 'Feb' @feb | 'Mar' @mar | 'Apr' @apr |
    'May' @may | 'Jun' @jun | 'Jul' @jul | 'Aug' @aug | 'Sep' @sep |
    'Oct' @oct | 'Nov' @nov | 'Dec' @dec);
  day_re = ((space . digit >clear_int $add_int ) | digit{2} > clear_int $add_int) @day;
  hour_re = (digit{2} > clear_int $add_int) @hour;
  min_re = (digit{2} > clear_int $add_int) @min;
  sec_re = (digit{2} > clear_int $add_int) @sec;
  date_time_re = month_re . space day_re . space . hour_re ':' min_re ':' sec_re;
}%%