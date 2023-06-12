(* chapter04 *)
(* 時給（円） *)
let jikyu = 950

(* 基本給（円） *)
let kihonkyu = 100

(* 目的：働いた時間 hour に応じたアルバイト代を計算する *)
(* kyuyo : int -> int *)
let kyuyo hour = kihonkyu + hour * jikyu

let test1 = kyuyo 25 = 23850
let test2 = kyuyo 28 = 26700
let test3 = kyuyo 31 = 29550

(* 目的：鶴の数 tsuru に応じた足の本数を返す *)
(* tsuru_no_ashi : int -> int *)
let tsuruFootNum = 2
let tsuru_no_ashi tsuru = tsuru * tsuruFootNum

let test1 = tsuru_no_ashi 0 = 0
let test2 = tsuru_no_ashi 2 = 4
let test3 = tsuru_no_ashi 10 = 20


(* 目的：亀の数 kame に応じた足の本数を返す *)
(* kame_no_ashi : int -> int *)
let kameFootNum = 4
let kame_no_ashi kame = kame * kameFootNum

let test1 = kame_no_ashi 0 = 0
let test2 = kame_no_ashi 2 = 8
let test3 = kame_no_ashi 10 = 40


(* 目的：鶴の数 tsuru 亀の数 kame に応じた合計の足の本数を返す *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi tsuru kame = tsuru_no_ashi tsuru + kame_no_ashi kame

let test1 = tsurukame_no_ashi 0 0 = 0
let test2 = tsurukame_no_ashi 2 1 = 8
let test3 = tsurukame_no_ashi 1 2 = 10

(* 目的：鶴と亀の合計数 headNum 足の数の合計数 footNum に応じて、鶴の数を返す *)
(* tsurukame : int -> int -> int *)
let tsurukame headNum footNum = headNum - (footNum / 2 - headNum)

let test1 = tsurukame 0 0 = 0
let test2 = tsurukame 3 8 = 2
let test3 = tsurukame 5 14 = 3