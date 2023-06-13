(* chapter04 *)
(* 時給（円） *)
let jikyu = 950

(* 基本給（円） *)
let kihonkyu = 100

(* 目的：働いた時間 hour に応じたアルバイト代を計算する *)
(* kyuyo : int -> int *)
let kyuyo hour = kihonkyu + (hour * jikyu)
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
let tsurukame headNum footNum = headNum - ((footNum / 2) - headNum)
let test1 = tsurukame 0 0 = 0
let test2 = tsurukame 3 8 = 2
let test3 = tsurukame 5 14 = 3

(* Chapter05 条件分岐 *)

(* 優遇時給 *)
let yugu_jikyu = 980

(* 目的：労働時間 hour を受け取り、給与を返す *)
(* kyuyo:int -> int *)
let kyuyo hour = kihonkyu + (hour * if hour < 30 then jikyu else yugu_jikyu)
let test1 = kyuyo 0 = 100
let test2 = kyuyo 20 = 19100
let test3 = kyuyo 40 = 39300

(* 目的：受け取った実数 x の絶対値を返す *)
(* abs_value: float -> float *)
let abs_value x = x *. if x > 0. then 1. else -1.
let test1 = abs_value 0. = 0.
let test2 = abs_value 1. = 1.
let test3 = abs_value (-3.) = 3.

(* 目的：時間を表すhhmm形式の文字列 hhmm を受け取り、午前か午後かを返す *)
(* string -> string *)
let jikan hhmm = if int_of_string hhmm < 1200 then "am" else "pm"
let test1 = jikan "0830" = "am"
let test2 = jikan "1230" = "pm"
let test3 = jikan "1200" = "pm"
let test4 = jikan "0000" = "am"

(* 目的：誕生日を表すMMDD形式の文字列 mmdd を受け取り、季節を返す関数kisetsu *)
let kisetsu mmdd =
  let month = int_of_string mmdd / 100 in
  if month < 4 then "spring"
  else if month < 7 then "summer"
  else if month < 10 then "autumn"
  else "winter"

let test1 = kisetsu "0120" = "spring"
let test2 = kisetsu "0420" = "summer"
let test3 = kisetsu "0720" = "autumn"
let test4 = kisetsu "1020" = "winter"

(* 目的：二次方程式の係数 a b c を受け取り、判別式の値を返す *)
(* float -> float -> float -> float *)
let hanbetsushiki a b c = (b ** 2.) -. (4. *. a *. c)

(* 二次方程式の解の個数を返す *)
(* float -> float -> float -> int *)
let kai_no_kosuu a b c =
  let d = hanbetsushiki a b c in
  if d > 0. then 2 else if d = 0. then 1 else 0

let test1 = kai_no_kosuu 1. 2. 1. = 1
let test2 = kai_no_kosuu 2. (-4.) (-30.) = 2
let test3 = kai_no_kosuu 1. 2. 100. = 0

(* 二次方程式が虚数解を持つかどうかを返す *)
(* float -> float -> float -> bool *)
let kyosuukai a b c =
  let d = hanbetsushiki a b c in
  if d < 0. then true else false

let test1 = kyosuukai 1. 2. 1. = false
let test2 = kyosuukai 2. (-4.) (-30.) = false
let test3 = kyosuukai 1. 2. 100. = true

(* 身長 height 体重 weightに対してbmiを返す *)
(* float -> float -> float *)
let bmiCalc height weight = weight /. (height ** 2.)

let taikei height weight =
  let bmi = bmiCalc height weight in
  if bmi < 18.5 then "yase" else if bmi < 25. then "standard" else "huto"

(* Chapter07 tupleとパターンマッチ *)

(* tupleは(a, b)の形で定義 *)
(* 目的：５教科の点数を与えられた時、合計点と平均点を組にして返す *)
(* goukei_to_heikin: int -> int -> int -> int -> int -> int * float *)
let goukei_to_heikin test1 test2 test3 test4 test5 =
  let sum = test1 + test2 + test3 + test4 + test5 in
  let ave = float_of_int sum /. 5. in
  (sum, ave)

let test = goukei_to_heikin 80 80 80 80 90 = (410, 82.)
let test = goukei_to_heikin 80 80 80 80 81 = (401, 80.2)

(* パターンマッチ：ラムダ式が使える *)
(* match タプル with パターン -> 式 *)
let add pair = match pair with a, b -> a + b
let add_3_5 = add (3, 5)

(* 目的：名前 name と成績 score を受けとり、name 's score is scoreという文字列を返す *)
(* string * int -> string *)
let seiseki nameAndScore =
  match nameAndScore with
  | name, score -> name ^ "s'Score is " ^ string_of_int score

let test1 = seiseki ("Hanako", 92)

(* 目的：x座標、y座標の値を実数の組として受け取り、x軸について戦対象な座標を返す *)
(* float * float -> float * float *)
let taisho_x pos = match pos with x, y -> (x, -.y)
let test1 = taisho_x (1., 2.)
let test1 = taisho_x (1., -2.)

(* 目的、x座標、y座標の値を格納した二つの組を受け取り、中点の座標を返す *)
(* (float*float) -> (float*float) -> float*float *)
(* カリー化することで、複数タプルの引数を扱える。matchの->以降はlet inのin以降のように捉えるといいかも *)
(* match tup1 with (t1,s1) -> match tup2 with (t2,s2) -> ... *)
let chuten pos1 pos2 =
  match pos1 with
  | x1, y1 -> (
      match pos2 with x2, y2 -> ((x1 +. x2) /. 2., (y1 +. y2) /. 2.))

(* Chapter08 レコード *)
(* レコード：構造体（名前付き値の集合） *)

(* まずtypeで型を定義する *)
type gakuseiType = { name : string; score : int; height : float }

(* 同じ構造のレコードを作成すると作成した型が自動で割り当てられる（構造的部分型） *)
let gakusei_t = { name = "John"; score = 90; height = 1.82 }

(* タプルの例と同様に、レコードにもパターンマッチによるラムダ式が使える *)
(* match レコード with {フィールド名=変数名;フィールド名=変数名;} *)
let tsuuchi gakusei =
  match gakusei with
  | { name = n; score = s; height = h } ->
      n ^ "'s score is " ^ string_of_int s ^ ". And his/her height is "
      ^ string_of_float h ^ "cm."

(* 人の名前、身長、体重、誕生日（mmdd）、血液型を格納する型 *)
type personType = {
  name : string;
  height : float;
  weight : float;
  birthDay : string;
  bloodType : string;
}

(* personType型の情報を受け取り、名前と血液型を示す文字列を返す *)
(* personType -> string *)
let showBloodType person =
  match person with
  | { name; height; weight; birthDay; bloodType } ->
      name ^ "'s blood type is " ^ bloodType ^ "."

let test1 =
  showBloodType
    {
      name = "James";
      height = 1.72;
      weight = 80.;
      birthDay = "1230";
      bloodType = "A";
    }
  = "James's blood type is A."

let test2 =
  showBloodType
    {
      name = "Tom";
      height = 1.72;
      weight = 80.;
      birthDay = "1230";
      bloodType = "B";
    }
  = "Tom's blood type is B."
