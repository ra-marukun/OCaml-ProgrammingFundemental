type stationType = {
  kanji : string;
  kana : string;
  roman : string;
  line : string;
}

type lineType = {
  start : string;
  goal : string;
  via : string;
  distance : float;
  time : int;
}

let showStation station =
  match station with
  | { kanji; kana; roman; line } -> line ^ ", " ^ kanji ^ "(" ^ kana ^ ")"

let msg =
  showStation
    { kanji = "茗荷谷"; kana = "みょうがだに"; roman = "myogadani"; line = "丸の内線" }
