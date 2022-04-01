(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** [plot_target] specifies where to display the plot. *)
type plot_target =
  | Save of {file : string option}
      (** Save to [file] in .pdf format if not [None],
          otherwise saves to "{bench-name}_{model-name}_{kind}.pdf" *)
  | Show  (** Display to screen (requires Qt) *)
  | ShowAndSave of {file : string option}  (** Combines previous options *)

(** [options] specifies some display parameters. *)
type options = {
  point_size : float;
      (** Specifies the size of points for scatter plots.
          Defaults to [0.5] *)
  qt_target_pixel_size : (int * int) option;
      (** Specifies the size, in pixels, of the window in which plots are performed.
          If set to [None], selected automatically by gnuplot. Defaults to
          [1920, 1080]. *)
  pdf_target_cm_size : (float * float) option;
      (** Specifies the size, in centimeters, of the image generated by gnuplot.
          If set to [None], selected automatically by gnuplot. [None] is the
          default. *)
  quantiles : float list option;
      (** If set, triggers displaying the specified quantiles instead of only the median
          for the empirical data. Quantiles must be comprised between [0] and [1].
          Defaults to [None]. *)
}

(** Encoding for options. *)
val options_encoding : options Data_encoding.t

(** Default options. See {!options} documentation. *)
val default_options : options

(** Performs the plot. *)
val perform_plot :
  measure:Measure.packed_measurement ->
  model_name:string ->
  problem:Inference.problem ->
  solution:Inference.solution ->
  plot_target:plot_target ->
  options:options ->
  bool
