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

(* Chapter09 リスト *)
(* a::listのようにすることで、[a,...list]というリストが作成できる *)
(* [a;b]でも普通にかける *)
let listPerson =
  [
    {
      name = "James";
      height = 1.72;
      weight = 80.;
      birthDay = "1230";
      bloodType = "A";
    };
    {
      name = "Tom";
      height = 1.72;
      weight = 80.;
      birthDay = "1230";
      bloodType = "B";
    };
  ]

(* リストに対してもパターンマッチできる *)
(* 注意：リストの場合は、match リスト with パターン -> 式　のパターン -> 式を複数個用意できる *)
(* 全パターンを網羅するようにマッチしないと実行前エラーとなる *)
let getFirstOfList lst = match lst with [] -> 0 | first :: rest -> first
let test1 = getFirstOfList [ 1; 2; 3 ] = 1
let test1 = getFirstOfList [ 20; 12; 13 ] = 20
let test1 = getFirstOfList [] = 0

(* 可変長であるリストをうまく扱うためには、再起的に取り扱うことが有効 *)

(* 目的：受け取ったリスト lst に 0 が含まれているかを調べる *)
(* containZero: int list -> bool *)
let rec containZero lst =
  (* letでなくlet recとして定義することで、再起的に使うことを明示 *)
  match lst with
  | [] -> false (* 停止性の条件として、空リストに対する結果を定義 *)
  | first :: rest ->
      if first = 0 then true
      else containZero rest (* 要素の一つ目が0のときtrue, そうでない時は残りの要素に対して再起的に実行 *)

let test1 = containZero [] = false
let test1 = containZero [ 1; 2; 0 ] = true
let test1 = containZero [ 1; 2; 3 ] = false

(* 目的：整数の入ったリストを受け取り、全ての要素の和を求める *)
(* sum: int list -> int *)
let rec sum lst = match lst with [] -> 0 | first :: rest -> first + sum rest
let test = sum [] = 0
let test = sum [ 0 ] = 0
let test = sum [ 10 ] = 10
let test = sum [ 10; 20 ] = 30
let test = sum [ 1; 2; 3; 4; 5 ] = 15

(* 目的：整数のリストを受け取り、リストの長さを求める *)
(* length: int list -> int *)
let rec length lst = match lst with [] -> 0 | first :: rest -> 1 + length rest
let test = length [] = 0
let test = length [ 1 ] = 1
let test = length [ 10 ] = 1
let test = length [ 10; 20 ] = 2
let test = length [ 1; 2; 3; 4; 5 ] = 5

(* 目的：整数のリストを受け取り、偶数の要素のみを含むリストを返す *)
(* int list -> int list *)
let rec even lst =
  match lst with
  | [] -> []
  | first :: rest -> if first mod 2 = 0 then first :: even rest else even rest

let test = even [] = []
let test = even [ 1 ] = []
let test = even [ 4 ] = [ 4 ]
let test = even [ 2; 5; 6; 8; 11; 12 ] = [ 2; 6; 8; 12 ]

(* 目的：文字列のリストを受け取り、要素を前から順に結合した文字列を返す *)
(* string list -> string *)
let rec concat lst =
  match lst with [] -> "" | first :: rest -> first ^ concat rest

let test = concat [] = ""
let test = concat [ "1" ] = "1"
let test = concat [ "春"; "なつ"; "winter" ] = "春なつwinter"
let test = concat [ "春"; ""; "winter" ] = "春winter"

(* 学生のリスト lst のうち、特定の成績の生徒の数を返す *)
(* gakuseiType list -> int *)
type gakuseiType = { name : string; score : int; rank : string }

let rec countByRank lst rank =
  match lst with
  | [] -> 0
  | { name = n; score = s; rank = r } :: rest ->
      if r = rank then 1 + countByRank rest rank else countByRank rest rank

let lst1 = []
let lst2 = [ { name = "john"; score = 85; rank = "B" } ]
let lst3 = [ { name = "john"; score = 95; rank = "A" } ]

let lst4 =
  [
    { name = "john"; score = 85; rank = "B" };
    { name = "john"; score = 95; rank = "A" };
  ]

let lst5 =
  [
    { name = "john"; score = 85; rank = "B" };
    { name = "john"; score = 95; rank = "A" };
    { name = "john"; score = 92; rank = "A" };
  ]

let test = countByRank lst1 "A" = 0
let test = countByRank lst2 "A" = 0
let test = countByRank lst3 "A" = 1
let test = countByRank lst4 "A" = 1
let test = countByRank lst5 "A" = 2
let test = countByRank lst5 "B" = 1

(* 目的：personType型のリストを受け取り、血液型がAの人の数を返す *)
(* personType list -> int *)
let rec countBloodTypeIsA lst =
  match lst with
  | [] -> 0
  | { name; height; weight; birthDay; bloodType } :: rest ->
      if bloodType = "A" then 1 + countBloodTypeIsA rest
      else countBloodTypeIsA rest

let lst1 = []

let lst2 =
  [
    {
      name = "james";
      height = 1.7;
      weight = 56.;
      birthDay = "1231";
      bloodType = "A";
    };
  ]

let lst3 =
  [
    {
      name = "john";
      height = 1.2;
      weight = 56.;
      birthDay = "1231";
      bloodType = "B";
    };
  ]

let lst4 =
  [
    {
      name = "bob";
      height = 1.6;
      weight = 56.;
      birthDay = "1231";
      bloodType = "A";
    };
    {
      name = "john";
      height = 1.2;
      weight = 56.;
      birthDay = "1231";
      bloodType = "B";
    };
  ]

let lst5 =
  [
    {
      name = "big";
      height = 1.7;
      weight = 56.;
      birthDay = "1231";
      bloodType = "A";
    };
    {
      name = "big";
      height = 1.8;
      weight = 56.;
      birthDay = "1231";
      bloodType = "AB";
    };
  ]

let lst6 =
  [
    {
      name = "big";
      height = 1.7;
      weight = 56.;
      birthDay = "1231";
      bloodType = "A";
    };
    {
      name = "small";
      height = 1.3;
      weight = 56.;
      birthDay = "1231";
      bloodType = "A";
    };
    {
      name = "big";
      height = 1.8;
      weight = 56.;
      birthDay = "1231";
      bloodType = "AB";
    };
  ]

let test = countBloodTypeIsA lst1 = 0
let test = countBloodTypeIsA lst2 = 1
let test = countBloodTypeIsA lst3 = 0
let test = countBloodTypeIsA lst4 = 1
let test = countBloodTypeIsA lst5 = 1
let test = countBloodTypeIsA lst6 = 2

(* 目的：personType型のリストを受け取り、身長が1.6m以上の人の名前を格納したリストを返す *)
(* personType list -> string list *)
let rec nameOfHeightIsOber1p6 lst =
  match lst with
  | [] -> []
  | { name; height; weight; birthDay; bloodType } :: rest ->
      if height < 1.6 then nameOfHeightIsOber1p6 rest
      else name :: nameOfHeightIsOber1p6 rest

let test = nameOfHeightIsOber1p6 lst1
let test = nameOfHeightIsOber1p6 lst2
let test = nameOfHeightIsOber1p6 lst3
let test = nameOfHeightIsOber1p6 lst4
let test = nameOfHeightIsOber1p6 lst5
let test = nameOfHeightIsOber1p6 lst6

(* Chapter10 再帰関数を使ったプログラミング *)
(* 目的：整数と整数リストのリストを受け取り、全ての整数リストの先頭に整数を付け加えたリストを返す *)
(* int -> int list list -> int list list *)
let rec add_to_each num lst =
  match lst with
  | [] -> []
  | first :: rest -> (num :: first) :: add_to_each num rest

let test = add_to_each 1 [] = []
let test = add_to_each 1 [ [ 2 ] ] = [ [ 1; 2 ] ]
let test = add_to_each 1 [ [ 2 ]; [ 2; 3 ] ] = [ [ 1; 2 ]; [ 1; 2; 3 ] ]

let test =
  add_to_each 1 [ [ 2 ]; [ 2; 3 ]; [ 2; 3; 4 ] ]
  = [ [ 1; 2 ]; [ 1; 2; 3 ]; [ 1; 2; 3; 4 ] ]

(* 目的：整数のリストを受け取り、その接頭語のリストをかえす *)
(* int list -> int list list *)
let rec prefix lst =
  match lst with
  | [] -> []
  | first :: rest -> [ first ] :: add_to_each first (prefix rest)

let test = prefix [] = []
let test = prefix [ 1 ] = [ [ 1 ] ]
let test = prefix [ 1; 2 ] = [ [ 1 ]; [ 1; 2 ] ]
let test = prefix [ 1; 2; 3 ] = [ [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ] ]

(* 目的：昇順に並んだ整数のリストlstと整数numを受け取り、昇順となるようにnumをlstに挿入する *)
(* int list -> int -> int list *)
let rec insert lst num =
  match lst with
  | [] -> [ num ]
  | first :: rest ->
      if num < first then num :: first :: rest else first :: insert rest num

let test = insert [] 1 = [ 1 ]
let test = insert [ 1 ] 10 = [ 1; 10 ]
let test = insert [ 1; 3; 4; 5; 10 ] 8 = [ 1; 3; 4; 5; 8; 10 ]
let test = insert [ 1; 3; 4; 5; 10 ] 5 = [ 1; 3; 4; 5; 5; 10 ]

(* 目的：整数のリストを受け取り、昇順にソートしたリストを返す *)
(* int list -> int list *)
let rec ins_sort lst =
  match lst with [] -> [] | first :: rest -> insert (ins_sort rest) first

let test = ins_sort [] = []
let test = ins_sort [ 6 ] = [ 6 ]
let test = ins_sort [ 1; 2; 10 ] = [ 1; 2; 10 ]
let test = ins_sort [ 2; 10; 1 ] = [ 1; 2; 10 ]
let test = ins_sort [ 7; 10; 2; 9 ] = [ 2; 7; 9; 10 ]
let test = ins_sort [ 2; 10; 7; 7 ] = [ 2; 7; 7; 10 ]

(* 目的：score順にsortされたgakuseiTypeのリスト lst と、同じ型の値gakuseiを受け取り、昇順を維持して挿入したリストを返す *)
(* gakuseiType list -> gakuseiType -> gakuseiType list *)
let rec gakusei_ins lst gakusei =
  match lst with
  | [] -> [ gakusei ]
  | ({ name = name_f; score = score_f; rank = rank_f } as first) :: rest -> (
      match gakusei with
      | { name = name_g; score = score_g; rank = rank_g } ->
          if score_g < score_f then gakusei :: first :: rest
          else first :: gakusei_ins rest gakusei)

(* 目的：gakuseiTypeのリストを受け取り、scoreフィールドの値で昇順にソートしたリストを返す *)
(* gakuseiType list -> gakuseiType list *)
let rec gakusei_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> gakusei_ins (gakusei_sort rest) first

let test = gakusei_sort [] = []
let test = gakusei_sort [ { name = "john"; score = 95; rank = "A" } ]

let test =
  gakusei_sort
    [
      { name = "john"; score = 85; rank = "B" };
      { name = "james"; score = 95; rank = "A" };
      { name = "bob"; score = 92; rank = "A" };
    ]

(* 目的：与えられた整数のリストの中から最小値を取り出す *)
(* int list -> int *)
(* 空の場合はmax_intという特殊な値を入れる。これはint型の最大値 *)
let rec minimum lst =
  match lst with
  | [] -> max_int
  | first :: rest -> if first < minimum rest then first else minimum rest

let test = minimum [ 1; 10; 5 ] = 1
let test = minimum [ 1; 10; -5 ] = -5
let test = minimum [ 999 ] = 999

(* 目的：学生のリストの中から最高得点の学生のレコードを返す *)
(* gakuseiType list -> gakuseiType list *)
let rec getGakuseiOfMaxScore lst =
  match lst with
  | [] -> { name = "foo"; score = min_int; rank = "Z" }
  | ({ name; score; rank } as first) :: rest -> (
      match getGakuseiOfMaxScore rest with
      | { name = name_r; score = score_r; rank = rank_r } as r ->
          if score > score_r then first else r)

let gakusei1 = { name = "nakamura"; score = 90; rank = "A" }
let gakusei2 = { name = "miyahara"; score = 80; rank = "A" }
let gakusei3 = { name = "sato"; score = 75; rank = "B" }
let gakusei4 = { name = "idehara"; score = 70; rank = "B" }
let gakusei5 = { name = "tsubata"; score = 65; rank = "C" }
let gakusei6 = { name = "asai"; score = 60; rank = "C" }

(* テスト *)
let lst1 = [ gakusei2 ]
let lst2 = [ gakusei3; gakusei4 ]
let lst3 = [ gakusei4; gakusei3 ]
let lst4 = [ gakusei4; gakusei1; gakusei6; gakusei5; gakusei2; gakusei3 ]
let test1 = getGakuseiOfMaxScore lst1 = gakusei2
let test2 = getGakuseiOfMaxScore lst2 = gakusei3
let test3 = getGakuseiOfMaxScore lst3 = gakusei3
let test4 = getGakuseiOfMaxScore lst4 = gakusei1

(* 局所変数定義 *)
(* let 変数 = 式1 in 式2　の形で、変数を式２の中だけで局所的に利用可能 *)

(* 先ほどのminimumを書き換える。(もともとminimum restを２回計算していたところを、一度だけにする) *)
let rec minimum lst =
  match lst with
  | [] -> max_int
  | first :: rest ->
      let min_rest = minimum rest in
      if first < min_rest then first else min_rest

let rec getGakuseiOfMaxScore lst =
  match lst with
  | [] -> { name = "foo"; score = min_int; rank = "Z" }
  | ({ name; score; rank } as first) :: rest -> (
      let max_rest = getGakuseiOfMaxScore rest in
      match max_rest with
      | { name = name_r; score = score_r; rank = rank_r } ->
          if score > score_r then first else max_rest)

let test1 = getGakuseiOfMaxScore lst1 = gakusei2
let test2 = getGakuseiOfMaxScore lst2 = gakusei3
let test3 = getGakuseiOfMaxScore lst3 = gakusei3
let test4 = getGakuseiOfMaxScore lst4 = gakusei1

(* gakuseiTypeのリストを受け取り、rankごとの人数をタプルとして返す *)
(* gakuseiType list -> int*int*int*int *)
let rec summarizeRank lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | { name; score; rank } :: rest -> (
      let sum_rest = summarizeRank rest in
      match sum_rest with
      | a, b, c, d ->
          if rank = "A" then (a + 1, b, c, d)
          else if rank = "B" then (a, b + 1, c, d)
          else if rank = "C" then (a, b, c + 1, d)
          else (a, b, c, d + 1))

let test1 = summarizeRank [] = (0, 0, 0, 0)
let test1 = summarizeRank lst1 = (1, 0, 0, 0)
let test1 = summarizeRank lst2 = (0, 2, 0, 0)
let test1 = summarizeRank lst3 = (0, 2, 0, 0)
let test1 = summarizeRank lst4 = (2, 2, 2, 0)

(* let 変数 = 式1 in 式2 の変数にはパターンもかける。その場合、パターンのmatch文は不要となる *)
let rec summarizeRank lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | { name; score; rank } :: rest ->
      let a, b, c, d = summarizeRank rest in
      if rank = "A" then (a + 1, b, c, d)
      else if rank = "B" then (a, b + 1, c, d)
      else if rank = "C" then (a, b, c + 1, d)
      else (a, b, c, d + 1)

(* personTypeのリストを受け取り、各血液型の人数をタプルにして返す *)
let rec sumBloodType lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | { name; height; weight; birthDay; bloodType } :: rest ->
      let a, b, ab, o = sumBloodType rest in
      if bloodType = "A" then (a + 1, b, ab, o)
      else if bloodType = "B" then (a, b + 1, ab, o)
      else if bloodType = "AB" then (a, b, ab + 1, o)
      else (a, b, ab, o + 1)

let lst1 = []

let lst2 =
  [
    {
      name = "james";
      height = 1.7;
      weight = 56.;
      birthDay = "1231";
      bloodType = "A";
    };
  ]

let lst3 =
  [
    {
      name = "john";
      height = 1.2;
      weight = 56.;
      birthDay = "1231";
      bloodType = "B";
    };
  ]

let lst4 =
  [
    {
      name = "bob";
      height = 1.6;
      weight = 56.;
      birthDay = "1231";
      bloodType = "A";
    };
    {
      name = "john";
      height = 1.2;
      weight = 56.;
      birthDay = "1231";
      bloodType = "B";
    };
  ]

let lst5 =
  [
    {
      name = "big";
      height = 1.7;
      weight = 56.;
      birthDay = "1231";
      bloodType = "B";
    };
    {
      name = "big";
      height = 1.8;
      weight = 56.;
      birthDay = "1231";
      bloodType = "O";
    };
  ]

let lst6 =
  [
    {
      name = "big";
      height = 1.7;
      weight = 56.;
      birthDay = "1231";
      bloodType = "A";
    };
    {
      name = "small";
      height = 1.3;
      weight = 56.;
      birthDay = "1231";
      bloodType = "A";
    };
    {
      name = "big";
      height = 1.8;
      weight = 56.;
      birthDay = "1231";
      bloodType = "AB";
    };
  ]

let test = sumBloodType lst1 = (0, 0, 0, 0)
let test = sumBloodType lst2 = (1, 0, 0, 0)
let test = sumBloodType lst3 = (0, 1, 0, 0)
let test = sumBloodType lst4 = (1, 1, 0, 0)
let test = sumBloodType lst5 = (0, 1, 0, 1)
let test = sumBloodType lst6 = (2, 0, 1, 0)

(* 整数のリストを受け取り、最大値を返す *)
let rec maximum lst =
  match lst with
  | [] -> min_int
  | first :: rest ->
      let max_rest = maximum rest in
      if first > max_rest then first else max_rest

(* 目的：personTypeのリストを受け取り、最も多い血液型を返す *)
let mostBloodType lst =
  let a, b, ab, o = sumBloodType lst in
  let max_blood = maximum [ a; b; ab; o ] in
  if max_blood = 0 then "No person in the list"
  else if max_blood = a then "A"
  else if max_blood = b then "B"
  else if max_blood = ab then "AB"
  else "O"

let test = mostBloodType lst1 = "No person in the list"
let test = mostBloodType lst2 = "A"
let test = mostBloodType lst3 = "B"
let test = mostBloodType lst4 = "A"
let test = mostBloodType lst5 = "B"
let test = mostBloodType lst6 = "A"

(* 目的：同じ型のリスト二つを受け取り、結合したリストを返す *)
(* 'a list -> 'a list -> 'a list *)
let rec append lst1 lst2 =
  match lst1 with [] -> lst2 | first :: rest -> first :: append rest lst2

let test = append [] [] = []
let test = append [] [ 1; 2 ] = [ 1; 2 ]
let test = append [ 1; 3 ] [] = [ 1; 3 ]
let test = append [ 1; 3 ] [ 2; 3 ] = [ 1; 3; 2; 3 ]
let test = append [ "a"; "b" ] [ "g"; "l" ] = [ "a"; "b"; "g"; "l" ]

(* 同じことはList.append lst1でもできるし、lst1 @ lst2でもOK *)
let app = List.append [ 1; 2 ] [ 5; 6 ] = [ 1; 2; 5; 6 ]
let app = [ "a"; "b" ] @ [ "c"; "f"; "g" ] = [ "a"; "b"; "c"; "f"; "g" ]

(* 昇順に並んだ二つのリスト lst1 lst2 を受け取り、結合して昇順に並んだリストを返す *)
let rec merge lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | first1 :: rest1 -> (
      match lst2 with
      | [] -> lst1
      | first2 :: rest2 ->
          if first1 < first2 then first1 :: merge rest1 lst2
          else first2 :: merge rest2 lst1)

let test = merge [] [] = []
let test = merge [] [ 1; 2 ] = [ 1; 2 ]
let test = merge [ 1; 3 ] [] = [ 1; 3 ]
let test = merge [ 1; 3 ] [ 2; 3 ] = [ 1; 2; 3; 3 ]
let test = merge [ 1; 5 ] [ 2; 3 ] = [ 1; 2; 3; 5 ]
let test = merge [ 2; 5 ] [ 1; 3 ] = [ 1; 2; 3; 5 ]

(* ２回matchさせなくても、二つのリストを組としてパターンマッチすることもできる *)
let rec merge lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> []
  | [], first2 :: rest2 -> lst2
  | first1 :: rest1, [] -> lst1
  | first1 :: rest1, first2 :: rest2 ->
      if first1 < first2 then first1 :: merge rest1 lst2
      else first2 :: merge rest2 lst1

let test = merge [] [] = []
let test = merge [] [ 1; 2 ] = [ 1; 2 ]
let test = merge [ 1; 3 ] [] = [ 1; 3 ]
let test = merge [ 1; 3 ] [ 2; 3 ] = [ 1; 2; 3; 3 ]
let test = merge [ 1; 5 ] [ 2; 3 ] = [ 1; 2; 3; 5 ]
let test = merge [ 2; 5 ] [ 1; 3 ] = [ 1; 2; 3; 5 ]

(* 目的：二つのリストを受け取り、それらの長さが同じかどうかを判定する *)
let rec lengthIsEqual lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> true
  | [], first :: rest -> false
  | first :: rest, [] -> false
  | first1 :: rest1, first2 :: rest2 -> lengthIsEqual rest1 rest2

(* Chapter11 自然数と再帰 *)
(* 自然数も再帰的に定義できるため、let recで取り扱う *)

(* 階乗を求める関数 *)
let rec fac n = if n = 0 then 1 else n * fac (n - 1)
let test = fac 0 = 1
let test = fac 1 = 1
let test = fac 3 = 6
let test = fac 10 = 3628800

(* mとnを受け取り、mのn乗を求める *)
let rec power m n = if n = 0 then 1 else m * power m (n - 1)
let test = power 3 0 = 1
let test = power 3 1 = 3
let test = power 3 3 = 27

(* ０から受け取った自然数までの自乗和を求める *)
let rec sum_of_square n = if n = 0 then 0 else (n * n) + sum_of_square (n - 1)
let test = sum_of_square 0 = 0
let test = sum_of_square 1 = 1
let test = sum_of_square 3 = 14

(* A(0)=3 A(n) = 2*A(n-1) -1  *)
let rec a n = if n = 0 then 3 else (2 * a (n - 1)) - 1
let test = a 0 = 3
let test = a 1 = 5
let test = a 2 = 9

(* Chapter13 一般化と高階関数 *)
(* 似ている関数を複数個作成している場合は、抽象化することを考えよう *)

(* 目的；実数のリストを受け取り、各要素の平方根のリストを返す *)
let rec map_sqrt lst =
  match lst with [] -> [] | first :: rest -> sqrt first :: map_sqrt rest

let hyouka gakusei =
  match gakusei with
  | { name; score; rank } ->
      let r =
        if score >= 90 then "A"
        else if score >= 80 then "B"
        else if score >= 70 then "C"
        else "D"
      in
      { name; score; rank = r }

(* gakuseiType listを受け取り、scoreに応じたrankに修正する *)
let rec map_hyouka lst =
  match lst with [] -> [] | first :: rest -> hyouka first :: map_hyouka rest

(* map_sqrtとmap_hyoukaは似ている。リストに対して適用している関数が違うだけ。 *)
(* map funcとして一般化する *)
let rec map func lst =
  match lst with [] -> [] | first :: rest -> func first :: map func rest

let test =
  let a = [ 1.; 2.; 3. ] in
  map_sqrt a = map sqrt a

(* 同じ機能のList.mapが存在する *)

(* personType listを受け取り、名前のリストを返す *)
let getPersonName person = match person with { name; height } -> name
let mapGetPersonName lst = List.map getPersonName lst

(* 関数を引数にしたり、返り値にすることもできる *)
(* 関数funcを受け取り、funcを２回適用する関数を返す *)
let twice func =
  let g x = func (func x) in
  g

let a_a_func a = a
let a_b_a_func a b = a
let a_b_b_func a b = b
let a__a_b__b_func a f = f a
let a_b__b_c__a_c_func f g a = g (f a)

(* 関数を二つ受け取り、二つの関数を合成した関数を返す *)
let compose f1 f2 =
  let g x = f1 (f2 x) in
  g

(* テスト *)
let add3 num = num + 3
let times2 num = num * 2
let test = compose times2 add3 4 = 14
let tt = twice twice

(* Chapter14 高階関数を使ったリスト処理 *)

(* filter *)
(* リストの中から条件を満たす要素を抽出 *)

(* 目的：int listから正の要素だけを抽出 *)
let rec filter_positive lst =
  match lst with
  | [] -> []
  | first :: rest ->
      if first > 0 then first :: filter_positive rest else filter_positive rest

(* 目的：int listから3で割った余が1の要素を抽出 *)
let rec filter_mod3_1 lst =
  match lst with
  | [] -> []
  | first :: rest ->
      if first mod 3 = 1 then first :: filter_mod3_1 rest
      else filter_mod3_1 rest

(* 上記はfilterを一般化できる *)
(* 目的：lstから条件pを満たす要素を抽出 *)
let rec filter p lst =
  match lst with
  | [] -> []
  | first :: rest -> if p first then first :: filter p rest else filter p rest

(* filterを使ってfilter_positiveを一般化 *)
let isPositive num = num > 0
let filter_positive lst = filter isPositive lst

(* 組み込み関数 List.filterを使ってもかける *)
let filter_positive lst = List.filter isPositive lst

(* 目的；整数要素を受け取り、偶数を抽出する *)
let isEven num = num mod 2 = 0
let even lst = List.filter isEven lst
let test = even [ 2; 3; 7; 8; 4 ] = [ 2; 8; 4 ]

(* gakuseiType listを受け取り、成績がAの人をかぞえる *)
let is_rankA gakusei = match gakusei with { name; score; rank } -> rank = "A"
let countA lst = List.length (List.filter is_rankA lst)

(* fold_right *)
(* リストの各要素をまとめて値を返す *)

(* int list を受け取り、和を返す *)
let rec sum lst = match lst with [] -> 0 | first :: rest -> first + sum rest
let test = sum [ 1; 4; 5 ] = 10

(* 'a listを受け取り、長さを返す *)
let rec length lst = match lst with [] -> 0 | first :: rest -> 1 + length rest
let test = length [ 1; 4; 5 ] = 3

(* 二つのlistを結合したlistを返す *)
let rec append lst1 lst2 =
  match lst1 with [] -> lst2 | first :: rest -> first :: append rest lst2

let test = append [ 1; 2 ] [ 5; 6; 8 ] = [ 1; 2; 5; 6; 8 ]

(* sumとlengthとappendは「空リストに対して値を一つ返す、そうでなければ再帰呼び出しを行い、その結果と先頭の要素を使って何らかの操作を行う」 *)
(* 目的：initから始めてlstの要素を右から順にfuncに入力する *)
(* funcを繰り返していくので、fold_right func lst initの型はfunc:'a ->'b ->'b lst:'a list init:'b *)
let rec fold_right func lst init =
  match lst with
  | [] -> init
  | first :: rest -> func first (fold_right func rest init)

(* fold_rightを使って sum length appendを再定義 *)
(* sum *)
let add_int first rest_result = first + rest_result
let sum lst = fold_right add_int lst 0
let test = sum [ 1; 4; 5 ] = 10

(* length *)
let add1 first rest_result = 1 + rest_result
let length lst = fold_right add1 lst 0
let test = length [ 1; 4; 5 ] = 3

(* append *)
let appendFirst first rest_result = first :: rest_result
let append lst1 lst2 = fold_right appendFirst lst1 lst2
let test = append [ 1; 3 ] [ 2; 5; 6 ] = [ 1; 3; 2; 5; 6 ]

(* string_count *)
let str_count first rest_result = String.length first + rest_result
let string_count lst = List.fold_right str_count lst 0
let test = string_count [ "Baby"; "is"; "coming" ] = 12

(* gakuseiType listを受け取り、全員の得点の合計を返す *)
let addGakuseiScore first rest_result =
  match first with { name; score; rank } -> score + rest_result

let gakusei_sum lst = List.fold_right addGakuseiScore lst 0
let test1 = gakusei_sum [] = 0
let test1 = gakusei_sum [ { name = "ss"; score = 20; rank = "A" } ] = 20

let test1 =
  gakusei_sum
    [
      { name = "ss"; score = 20; rank = "A" };
      { name = "ss"; score = 15; rank = "A" };
      { name = "ss"; score = 50; rank = "A" };
    ]
  = 85

(* 局所関数定義：局所変数と同じように関数を定義できる *)
let append lst1 lst2 =
  let appendFirst1 first rest_result = first :: rest_result in
  List.fold_right appendFirst1 lst1 lst2

(* nameless function *)
(* fun 引数 -> 式 とすることで無名関数を作れる *)

(* sum *)
let sum lst =
  List.fold_right (fun first rest_result -> first + rest_result) lst 0

(* append *)
let append lst1 lst2 =
  List.fold_right (fun first rest_result -> first :: rest_result) lst1 lst2

(* 整数を受け取り、1からその整数までに含まれる完全数を求める *)
let perfect n =
  let rec enumerate num = if num = 0 then [] else num :: enumerate (num - 1) in
  let divisor num = List.filter (fun x -> num mod x = 0) (enumerate num) in
  let sum lst = List.fold_right ( + ) lst 0 in
  List.filter (fun x -> sum (divisor x) - x = x) (enumerate n)

let test = perfect 6 = [ 6 ]
(* let test = perfect 10000 = [ 8128; 496; 28; 6 ] *)

(* 目的：1から受け取ったintまでの合計を求める関数 *)
let one_to_n n =
  let rec enumerate num = if num = 0 then [] else num :: enumerate (num - 1) in
  List.fold_right ( + ) (enumerate n) 0

let test = one_to_n 5 = 15

(* 受け取ったintの階乗を求める *)
let fac n =
  let rec enumerate num = if num = 0 then [] else num :: enumerate (num - 1) in
  List.fold_right ( * ) (enumerate n) 1

let test = fac 5 = 120

(* Chapter15 新しい形の再帰 *)
(* 構造に従った再帰でない場合の再起を考える *)

(* クイックソート *)

(* 補助関数：lstからnよりpである要素のみを取り出す *)
let take n lst p = List.filter (fun x -> p x n) lst

(* 補助関数:lstとintを受け取り、intより小さいlst要素を取り出す *)
let take_less n lst = take n lst ( < )

(* 補助関数:lstとintを受け取り、int以上のlst要素を取り出す *)
let take_greater n lst = take n lst ( >= )

let rec quick_sort lst =
  match lst with
  | [] -> []
  | first :: rest ->
      quick_sort (take_less first rest)
      @ [ first ]
      @ quick_sort (take_greater first rest)

let test = quick_sort [] = []
let test = quick_sort [ 1 ] = [ 1 ]
let test = quick_sort [ 1; 2 ] = [ 1; 2 ]
let test = quick_sort [ 2; 1 ] = [ 1; 2 ]
let test = quick_sort [ 5; 4; 9; 2; 8; 3 ] = [ 2; 3; 4; 5; 8; 9 ]
let test = quick_sort [ 5; 4; 9; 2; 8; 3; 9 ] = [ 2; 3; 4; 5; 8; 9; 9 ]

(* ユークリッドの互除法 *)
(* 二つの自然数 m n の最大公約数を求める *)
let rec gcd m n = if n = 0 then m else gcd n (m mod n)
let test = gcd 54 21 = 3
let test = gcd 120 72 = 24

(* エラストステネスのふるい *)
(* 自然数 n　以下の素数を全て求める *)

(* 補助関数；2以上n以下の自然数のリストを受け取ったら、同じ範囲の素数リストを返す *)
let rec sieve lst =
  match lst with
  | [] -> []
  | first :: rest ->
      first :: sieve (List.filter (fun x -> x mod first != 0) rest)

let test = sieve [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ] = [ 2; 3; 5; 7 ]

(* 自然数を受け取ったらそれ以下の素数のリストを返す *)
let prime n =
  let rec enumerateFrom2 num =
    if num = 2 then [ 2 ] else enumerateFrom2 (num - 1) @ [ num ]
  in
  sieve (enumerateFrom2 n)

let test = prime 10 = [ 2; 3; 5; 7 ]
