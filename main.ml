(*
 * Copyright © 2023 Sam Henke
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the “Software”), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 *)

open Base

let char_to_digit c = (Char.to_int c) - (Char.to_int '0')

let calibration_value s =
    let digits =
        String.to_list s
        |> List.filter_map ~f:(fun c ->
                if Char.is_digit c
                then Some (char_to_digit c)
                else None)
    in
    let tens_digit = List.hd_exn digits in
    let ones_digit = List.last_exn digits in
    ones_digit + 10*tens_digit

let () =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:calibration_value
    |> List.reduce_exn ~f:(+)
    |> Stdlib.print_int;
    Stdlib.print_newline ()
