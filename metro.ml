(* 駅を表現する型 *)
type stationType = {
  kanji : string;
  kana : string;
  romaji : string;
  shozoku : string;
}

(* 駅間の経路を表現する型 *)
type pathType = {
  kiten : string;
  shuten : string;
  keiyu : string;
  kyori : float;
  jikan : int;
}

(* ダイクストラ法のために、駅名とその駅までの最短距離、最短経路を保持するレコードのための型を作成 *)
type ekiType = {
  namae : string;
  saitan_kyori : float;
  temae_list : string list;
}

let showStation station =
  match station with
  | { kanji; kana; romaji; shozoku } ->
      shozoku ^ ", " ^ kanji ^ "(" ^ kana ^ ")"

(* 利用するデータ stationTypeのリスト *)
let global_ekimei_list =
  [
    {
      kanji = "代々木上原";
      kana = "よよぎうえはら";
      romaji = "yoyogiuehara";
      shozoku = "千代田線";
    };
    {
      kanji = "代々木公園";
      kana = "よよぎこうえん";
      romaji = "yoyogikouen";
      shozoku = "千代田線";
    };
    {
      kanji = "明治神宮前";
      kana = "めいじじんぐうまえ";
      romaji = "meijijinguumae";
      shozoku = "千代田線";
    };
    {
      kanji = "表参道";
      kana = "おもてさんどう";
      romaji = "omotesandou";
      shozoku = "千代田線";
    };
    { kanji = "乃木坂"; kana = "のぎざか"; romaji = "nogizaka"; shozoku = "千代田線" };
    { kanji = "赤坂"; kana = "あかさか"; romaji = "akasaka"; shozoku = "千代田線" };
    {
      kanji = "国会議事堂前";
      kana = "こっかいぎじどうまえ";
      romaji = "kokkaigijidoumae";
      shozoku = "千代田線";
    };
    {
      kanji = "霞ヶ関";
      kana = "かすみがせき";
      romaji = "kasumigaseki";
      shozoku = "千代田線";
    };
    { kanji = "日比谷"; kana = "ひびや"; romaji = "hibiya"; shozoku = "千代田線" };
    {
      kanji = "二重橋前";
      kana = "にじゅうばしまえ";
      romaji = "nijuubasimae";
      shozoku = "千代田線";
    };
    { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "千代田線" };
    {
      kanji = "新御茶ノ水";
      kana = "しんおちゃのみず";
      romaji = "shin-ochanomizu";
      shozoku = "千代田線";
    };
    { kanji = "湯島"; kana = "ゆしま"; romaji = "yushima"; shozoku = "千代田線" };
    { kanji = "根津"; kana = "ねづ"; romaji = "nedu"; shozoku = "千代田線" };
    { kanji = "千駄木"; kana = "せんだぎ"; romaji = "sendagi"; shozoku = "千代田線" };
    {
      kanji = "西日暮里";
      kana = "にしにっぽり";
      romaji = "nishinippori";
      shozoku = "千代田線";
    };
    { kanji = "町屋"; kana = "まちや"; romaji = "machiya"; shozoku = "千代田線" };
    { kanji = "北千住"; kana = "きたせんじゅ"; romaji = "kitasenjyu"; shozoku = "千代田線" };
    { kanji = "綾瀬"; kana = "あやせ"; romaji = "ayase"; shozoku = "千代田線" };
    { kanji = "北綾瀬"; kana = "きたあやせ"; romaji = "kitaayase"; shozoku = "千代田線" };
    { kanji = "浅草"; kana = "あさくさ"; romaji = "asakusa"; shozoku = "銀座線" };
    { kanji = "田原町"; kana = "たわらまち"; romaji = "tawaramachi"; shozoku = "銀座線" };
    { kanji = "稲荷町"; kana = "いなりちょう"; romaji = "inaricho"; shozoku = "銀座線" };
    { kanji = "上野"; kana = "うえの"; romaji = "ueno"; shozoku = "銀座線" };
    {
      kanji = "上野広小路";
      kana = "うえのひろこうじ";
      romaji = "uenohirokoji";
      shozoku = "銀座線";
    };
    { kanji = "末広町"; kana = "すえひろちょう"; romaji = "suehirocho"; shozoku = "銀座線" };
    { kanji = "神田"; kana = "かんだ"; romaji = "kanda"; shozoku = "銀座線" };
    {
      kanji = "三越前";
      kana = "みつこしまえ";
      romaji = "mitsukoshimae";
      shozoku = "銀座線";
    };
    { kanji = "日本橋"; kana = "にほんばし"; romaji = "nihonbashi"; shozoku = "銀座線" };
    { kanji = "京橋"; kana = "きょうばし"; romaji = "kyobashi"; shozoku = "銀座線" };
    { kanji = "銀座"; kana = "ぎんざ"; romaji = "ginza"; shozoku = "銀座線" };
    { kanji = "新橋"; kana = "しんばし"; romaji = "shinbashi"; shozoku = "銀座線" };
    { kanji = "虎ノ門"; kana = "とらのもん"; romaji = "toranomon"; shozoku = "銀座線" };
    {
      kanji = "溜池山王";
      kana = "ためいけさんのう";
      romaji = "tameikesannou";
      shozoku = "銀座線";
    };
    {
      kanji = "赤坂見附";
      kana = "あかさかみつけ";
      romaji = "akasakamitsuke";
      shozoku = "銀座線";
    };
    {
      kanji = "青山一丁目";
      kana = "あおやまいっちょうめ";
      romaji = "aoyamaicchome";
      shozoku = "銀座線";
    };
    { kanji = "外苑前"; kana = "がいえんまえ"; romaji = "gaienmae"; shozoku = "銀座線" };
    { kanji = "表参道"; kana = "おもてさんどう"; romaji = "omotesando"; shozoku = "銀座線" };
    { kanji = "渋谷"; kana = "しぶや"; romaji = "shibuya"; shozoku = "銀座線" };
    { kanji = "渋谷"; kana = "しぶや"; romaji = "shibuya"; shozoku = "半蔵門線" };
    {
      kanji = "表参道";
      kana = "おもてさんどう";
      romaji = "omotesandou";
      shozoku = "半蔵門線";
    };
    {
      kanji = "青山一丁目";
      kana = "あおやまいっちょうめ";
      romaji = "aoyama-itchome";
      shozoku = "半蔵門線";
    };
    { kanji = "永田町"; kana = "ながたちょう"; romaji = "nagatacho"; shozoku = "半蔵門線" };
    { kanji = "半蔵門"; kana = "はんぞうもん"; romaji = "hanzomon"; shozoku = "半蔵門線" };
    { kanji = "九段下"; kana = "くだんした"; romaji = "kudanshita"; shozoku = "半蔵門線" };
    { kanji = "神保町"; kana = "じんぼうちょう"; romaji = "jinbocho"; shozoku = "半蔵門線" };
    { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "半蔵門線" };
    {
      kanji = "三越前";
      kana = "みつこしまえ";
      romaji = "mitsukoshimae";
      shozoku = "半蔵門線";
    };
    {
      kanji = "水天宮前";
      kana = "すいてんぐうまえ";
      romaji = "suitengumae";
      shozoku = "半蔵門線";
    };
    {
      kanji = "清澄白河";
      kana = "きよすみしらかわ";
      romaji = "kiyosumi-shirakawa";
      shozoku = "半蔵門線";
    };
    { kanji = "住吉"; kana = "すみよし"; romaji = "sumiyoshi"; shozoku = "半蔵門線" };
    { kanji = "錦糸町"; kana = "きんしちょう"; romaji = "kinshicho"; shozoku = "半蔵門線" };
    { kanji = "押上"; kana = "おしあげ"; romaji = "oshiage"; shozoku = "半蔵門線" };
    { kanji = "中目黒"; kana = "なかめぐろ"; romaji = "nakameguro"; shozoku = "日比谷線" };
    { kanji = "恵比寿"; kana = "えびす"; romaji = "ebisu"; shozoku = "日比谷線" };
    { kanji = "広尾"; kana = "ひろお"; romaji = "hiro"; shozoku = "日比谷線" };
    { kanji = "六本木"; kana = "ろっぽんぎ"; romaji = "roppongi"; shozoku = "日比谷線" };
    { kanji = "神谷町"; kana = "かみやちょう"; romaji = "kamiyacho"; shozoku = "日比谷線" };
    {
      kanji = "霞ヶ関";
      kana = "かすみがせき";
      romaji = "kasumigaseki";
      shozoku = "日比谷線";
    };
    { kanji = "日比谷"; kana = "ひびや"; romaji = "hibiya"; shozoku = "日比谷線" };
    { kanji = "銀座"; kana = "ぎんざ"; romaji = "ginza"; shozoku = "日比谷線" };
    {
      kanji = "東銀座";
      kana = "ひがしぎんざ";
      romaji = "higashiginza";
      shozoku = "日比谷線";
    };
    { kanji = "築地"; kana = "つきじ"; romaji = "tsukiji"; shozoku = "日比谷線" };
    { kanji = "八丁堀"; kana = "はっちょうぼり"; romaji = "hacchobori"; shozoku = "日比谷線" };
    { kanji = "茅場町"; kana = "かやばちょう"; romaji = "kayabacho"; shozoku = "日比谷線" };
    {
      kanji = "人形町";
      kana = "にんぎょうちょう";
      romaji = "ningyomachi";
      shozoku = "日比谷線";
    };
    {
      kanji = "小伝馬町";
      kana = "こでんまちょう";
      romaji = "kodemmacho";
      shozoku = "日比谷線";
    };
    { kanji = "秋葉原"; kana = "あきはばら"; romaji = "akihabara"; shozoku = "日比谷線" };
    {
      kanji = "仲御徒町";
      kana = "なかおかちまち";
      romaji = "nakaokachimachi";
      shozoku = "日比谷線";
    };
    { kanji = "上野"; kana = "うえの"; romaji = "ueno"; shozoku = "日比谷線" };
    { kanji = "入谷"; kana = "いりや"; romaji = "iriya"; shozoku = "日比谷線" };
    { kanji = "三ノ輪"; kana = "みのわ"; romaji = "minowa"; shozoku = "日比谷線" };
    {
      kanji = "南千住";
      kana = "みなみせんじゅ";
      romaji = "minamisenju";
      shozoku = "日比谷線";
    };
    { kanji = "北千住"; kana = "きたせんじゅ"; romaji = "kitasenju"; shozoku = "日比谷線" };
    { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
    { kanji = "新大塚"; kana = "しんおおつか"; romaji = "shinotsuka"; shozoku = "丸ノ内線" };
    { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
    {
      kanji = "本郷三丁目";
      kana = "ほんごうさんちょうめ";
      romaji = "hongosanchome";
      shozoku = "丸ノ内線";
    };
    { kanji = "御茶ノ水"; kana = "おちゃのみず"; romaji = "ochanomizu"; shozoku = "丸ノ内線" };
    { kanji = "淡路町"; kana = "あわじちょう"; romaji = "awajicho"; shozoku = "丸ノ内線" };
    { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "丸ノ内線" };
    { kanji = "東京"; kana = "とうきょう"; romaji = "tokyo"; shozoku = "丸ノ内線" };
    { kanji = "銀座"; kana = "ぎんざ"; romaji = "ginza"; shozoku = "丸ノ内線" };
    {
      kanji = "霞ヶ関";
      kana = "かすみがせき";
      romaji = "kasumigaseki";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "国会議事堂前";
      kana = "こっかいぎじどうまえ";
      romaji = "kokkaigijidomae";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "赤坂見附";
      kana = "あかさかみつけ";
      romaji = "akasakamitsuke";
      shozoku = "丸ノ内線";
    };
    { kanji = "四ツ谷"; kana = "よつや"; romaji = "yotsuya"; shozoku = "丸ノ内線" };
    {
      kanji = "四谷三丁目";
      kana = "よつやさんちょうめ";
      romaji = "yotsuyasanchome";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "新宿御苑前";
      kana = "しんじゅくぎょえんまえ";
      romaji = "shinjuku-gyoemmae";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "新宿三丁目";
      kana = "しんじゅくさんちょうめ";
      romaji = "shinjuku-sanchome";
      shozoku = "丸ノ内線";
    };
    { kanji = "新宿"; kana = "しんじゅく"; romaji = "shinjuku"; shozoku = "丸ノ内線" };
    {
      kanji = "西新宿";
      kana = "にししんじゅく";
      romaji = "nishi-shinjuku";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "中野坂上";
      kana = "なかのさかうえ";
      romaji = "nakano-sakaue";
      shozoku = "丸ノ内線";
    };
    { kanji = "新中野"; kana = "しんなかの"; romaji = "shin-nakano"; shozoku = "丸ノ内線" };
    {
      kanji = "東高円寺";
      kana = "ひがしこうえんじ";
      romaji = "higashi-koenji";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "新高円寺";
      kana = "しんこうえんじ";
      romaji = "shin-koenji";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "南阿佐ヶ谷";
      kana = "みなみあさがや";
      romaji = "minami-asagaya";
      shozoku = "丸ノ内線";
    };
    { kanji = "荻窪"; kana = "おぎくぼ"; romaji = "ogikubo"; shozoku = "丸ノ内線" };
    {
      kanji = "中野新橋";
      kana = "なかのしんばし";
      romaji = "nakano-shimbashi";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "中野富士見町";
      kana = "なかのふじみちょう";
      romaji = "nakano-fujimicho";
      shozoku = "丸ノ内線";
    };
    { kanji = "方南町"; kana = "ほうなんちょう"; romaji = "honancho"; shozoku = "丸ノ内線" };
    { kanji = "四ツ谷"; kana = "よつや"; romaji = "yotsuya"; shozoku = "南北線" };
    { kanji = "永田町"; kana = "ながたちょう"; romaji = "nagatacho"; shozoku = "南北線" };
    {
      kanji = "溜池山王";
      kana = "ためいけさんのう";
      romaji = "tameikesanno";
      shozoku = "南北線";
    };
    {
      kanji = "六本木一丁目";
      kana = "ろっぽんぎいっちょうめ";
      romaji = "roppongiitchome";
      shozoku = "南北線";
    };
    {
      kanji = "麻布十番";
      kana = "あざぶじゅうばん";
      romaji = "azabujuban";
      shozoku = "南北線";
    };
    {
      kanji = "白金高輪";
      kana = "しろかねたかなわ";
      romaji = "shirokanetakanawa";
      shozoku = "南北線";
    };
    { kanji = "白金台"; kana = "しろかねだい"; romaji = "shirokanedai"; shozoku = "南北線" };
    { kanji = "目黒"; kana = "めぐろ"; romaji = "meguro"; shozoku = "南北線" };
    { kanji = "市ヶ谷"; kana = "いちがや"; romaji = "ichigaya"; shozoku = "南北線" };
    { kanji = "飯田橋"; kana = "いいだばし"; romaji = "idabashi"; shozoku = "南北線" };
    { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "南北線" };
    { kanji = "東大前"; kana = "とうだいまえ"; romaji = "todaimae"; shozoku = "南北線" };
    { kanji = "本駒込"; kana = "ほんこまごめ"; romaji = "honkomagome"; shozoku = "南北線" };
    { kanji = "駒込"; kana = "こまごめ"; romaji = "komagome"; shozoku = "南北線" };
    { kanji = "西ヶ原"; kana = "にしがはら"; romaji = "nishigahara"; shozoku = "南北線" };
    { kanji = "王子"; kana = "おうじ"; romaji = "oji"; shozoku = "南北線" };
    { kanji = "王子神谷"; kana = "おうじかみや"; romaji = "ojikamiya"; shozoku = "南北線" };
    { kanji = "志茂"; kana = "しも"; romaji = "shimo"; shozoku = "南北線" };
    {
      kanji = "赤羽岩淵";
      kana = "あかばねいわぶち";
      romaji = "akabaneiwabuchi";
      shozoku = "南北線";
    };
    {
      kanji = "西船橋";
      kana = "にしふなばし";
      romaji = "nishi-funabashi";
      shozoku = "東西線";
    };
    {
      kanji = "原木中山";
      kana = "ばらきなかやま";
      romaji = "baraki-nakayama";
      shozoku = "東西線";
    };
    { kanji = "妙典"; kana = "みょうでん"; romaji = "myoden"; shozoku = "東西線" };
    { kanji = "行徳"; kana = "ぎょうとく"; romaji = "gyotoku"; shozoku = "東西線" };
    {
      kanji = "南行徳";
      kana = "みなみぎょうとく";
      romaji = "minami-gyotoku";
      shozoku = "東西線";
    };
    { kanji = "浦安"; kana = "うらやす"; romaji = "urayasu"; shozoku = "東西線" };
    { kanji = "葛西"; kana = "かさい"; romaji = "kasai"; shozoku = "東西線" };
    { kanji = "西葛西"; kana = "にしかさい"; romaji = "nishi-kasai"; shozoku = "東西線" };
    {
      kanji = "南砂町";
      kana = "みなみすなまち";
      romaji = "minami-sunamachi";
      shozoku = "東西線";
    };
    { kanji = "東陽町"; kana = "とうようちょう"; romaji = "touyoucho"; shozoku = "東西線" };
    { kanji = "木場"; kana = "きば"; romaji = "kiba"; shozoku = "東西線" };
    {
      kanji = "門前仲町";
      kana = "もんぜんなかちょう";
      romaji = "monzen-nakacho";
      shozoku = "東西線";
    };
    { kanji = "茅場町"; kana = "かやばちょう"; romaji = "kayabacho"; shozoku = "東西線" };
    { kanji = "日本橋"; kana = "にほんばし"; romaji = "nihonbashi"; shozoku = "東西線" };
    { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "東西線" };
    { kanji = "竹橋"; kana = "たけばし"; romaji = "takebashi"; shozoku = "東西線" };
    { kanji = "九段下"; kana = "くだんした"; romaji = "kudanshita"; shozoku = "東西線" };
    { kanji = "飯田橋"; kana = "いいだばし"; romaji = "iidabashi"; shozoku = "東西線" };
    { kanji = "神楽坂"; kana = "かぐらざか"; romaji = "kagurazaka"; shozoku = "東西線" };
    { kanji = "早稲田"; kana = "わせだ"; romaji = "waseda"; shozoku = "東西線" };
    {
      kanji = "高田馬場";
      kana = "たかだのばば";
      romaji = "takadanobaba";
      shozoku = "東西線";
    };
    { kanji = "落合"; kana = "おちあい"; romaji = "ochiai"; shozoku = "東西線" };
    { kanji = "中野"; kana = "なかの"; romaji = "nakano"; shozoku = "東西線" };
    { romaji = "shinkiba"; kana = "しんきば"; kanji = "新木場"; shozoku = "有楽町線" };
    { romaji = "tatsumi"; kana = "たつみ"; kanji = "辰巳"; shozoku = "有楽町線" };
    { romaji = "toyosu"; kana = "とよす"; kanji = "豊洲"; shozoku = "有楽町線" };
    { romaji = "tsukishima"; kana = "つきしま"; kanji = "月島"; shozoku = "有楽町線" };
    {
      romaji = "shintomityou";
      kana = "しんとみちょう";
      kanji = "新富町";
      shozoku = "有楽町線";
    };
    {
      romaji = "ginzaittyoume";
      kana = "ぎんざいっちょうめ";
      kanji = "銀座一丁目";
      shozoku = "有楽町線";
    };
    {
      romaji = "yuurakutyou";
      kana = "ゆうらくちょう";
      kanji = "有楽町";
      shozoku = "有楽町線";
    };
    { romaji = "sakuradamon"; kana = "さくらだもん"; kanji = "桜田門"; shozoku = "有楽町線" };
    { romaji = "nagatacho"; kana = "ながたちょう"; kanji = "永田町"; shozoku = "有楽町線" };
    { romaji = "koujimachi"; kana = "こうじまち"; kanji = "麹町"; shozoku = "有楽町線" };
    { romaji = "ichigaya"; kana = "いちがや"; kanji = "市ヶ谷"; shozoku = "有楽町線" };
    { romaji = "iidabashi"; kana = "いいだばし"; kanji = "飯田橋"; shozoku = "有楽町線" };
    {
      kanji = "江戸川橋";
      kana = "えどがわばし";
      romaji = "edogawabasi";
      shozoku = "有楽町線";
    };
    { kanji = "護国寺"; kana = "ごこくじ"; romaji = "gokokuji"; shozoku = "有楽町線" };
    {
      kanji = "東池袋";
      kana = "ひがしいけぶくろ";
      romaji = "higasiikebukuro";
      shozoku = "有楽町線";
    };
    { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "有楽町線" };
    { kanji = "要町"; kana = "かなめちょう"; romaji = "kanametyou"; shozoku = "有楽町線" };
    { kanji = "千川"; kana = "せんかわ"; romaji = "senkawa"; shozoku = "有楽町線" };
    {
      kanji = "小竹向原";
      kana = "こたけむかいはら";
      romaji = "kotakemukaihara";
      shozoku = "有楽町線";
    };
    { kanji = "氷川台"; kana = "ひかわだい"; romaji = "hikawadai"; shozoku = "有楽町線" };
    { kanji = "平和台"; kana = "へいわだい"; romaji = "heiwadai"; shozoku = "有楽町線" };
    {
      kanji = "営団赤塚";
      kana = "えいだんあかつか";
      romaji = "eidanakakuka";
      shozoku = "有楽町線";
    };
    {
      kanji = "営団成増";
      kana = "えいだんなります";
      romaji = "eidannarimasu";
      shozoku = "有楽町線";
    };
    { kanji = "和光市"; kana = "わこうし"; romaji = "wakousi"; shozoku = "有楽町線" };
  ]

(* 利用するデータpathTypeのリスト *)
let global_ekikan_list =
  [
    {
      kiten = "代々木上原";
      shuten = "代々木公園";
      keiyu = "千代田線";
      kyori = 1.0;
      jikan = 2;
    };
    {
      kiten = "代々木公園";
      shuten = "明治神宮前";
      keiyu = "千代田線";
      kyori = 1.2;
      jikan = 2;
    };
    { kiten = "明治神宮前"; shuten = "表参道"; keiyu = "千代田線"; kyori = 0.9; jikan = 2 };
    { kiten = "表参道"; shuten = "乃木坂"; keiyu = "千代田線"; kyori = 1.4; jikan = 3 };
    { kiten = "乃木坂"; shuten = "赤坂"; keiyu = "千代田線"; kyori = 1.1; jikan = 2 };
    { kiten = "赤坂"; shuten = "国会議事堂前"; keiyu = "千代田線"; kyori = 0.8; jikan = 1 };
    { kiten = "国会議事堂前"; shuten = "霞ヶ関"; keiyu = "千代田線"; kyori = 0.7; jikan = 1 };
    { kiten = "霞ヶ関"; shuten = "日比谷"; keiyu = "千代田線"; kyori = 1.2; jikan = 2 };
    { kiten = "日比谷"; shuten = "二重橋前"; keiyu = "千代田線"; kyori = 0.7; jikan = 1 };
    { kiten = "二重橋前"; shuten = "大手町"; keiyu = "千代田線"; kyori = 0.7; jikan = 1 };
    { kiten = "大手町"; shuten = "新御茶ノ水"; keiyu = "千代田線"; kyori = 1.3; jikan = 2 };
    { kiten = "新御茶ノ水"; shuten = "湯島"; keiyu = "千代田線"; kyori = 1.2; jikan = 2 };
    { kiten = "湯島"; shuten = "根津"; keiyu = "千代田線"; kyori = 1.2; jikan = 2 };
    { kiten = "根津"; shuten = "千駄木"; keiyu = "千代田線"; kyori = 1.0; jikan = 2 };
    { kiten = "千駄木"; shuten = "西日暮里"; keiyu = "千代田線"; kyori = 0.9; jikan = 1 };
    { kiten = "西日暮里"; shuten = "町屋"; keiyu = "千代田線"; kyori = 1.7; jikan = 2 };
    { kiten = "町屋"; shuten = "北千住"; keiyu = "千代田線"; kyori = 2.6; jikan = 3 };
    { kiten = "北千住"; shuten = "綾瀬"; keiyu = "千代田線"; kyori = 2.5; jikan = 3 };
    { kiten = "綾瀬"; shuten = "北綾瀬"; keiyu = "千代田線"; kyori = 2.1; jikan = 4 };
    { kiten = "浅草"; shuten = "田原町"; keiyu = "銀座線"; kyori = 0.8; jikan = 2 };
    { kiten = "田原町"; shuten = "稲荷町"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 };
    { kiten = "稲荷町"; shuten = "上野"; keiyu = "銀座線"; kyori = 0.7; jikan = 2 };
    { kiten = "上野"; shuten = "上野広小路"; keiyu = "銀座線"; kyori = 0.5; jikan = 2 };
    { kiten = "上野広小路"; shuten = "末広町"; keiyu = "銀座線"; kyori = 0.6; jikan = 1 };
    { kiten = "末広町"; shuten = "神田"; keiyu = "銀座線"; kyori = 1.1; jikan = 2 };
    { kiten = "神田"; shuten = "三越前"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 };
    { kiten = "三越前"; shuten = "日本橋"; keiyu = "銀座線"; kyori = 0.6; jikan = 2 };
    { kiten = "日本橋"; shuten = "京橋"; keiyu = "銀座線"; kyori = 0.7; jikan = 2 };
    { kiten = "京橋"; shuten = "銀座"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 };
    { kiten = "銀座"; shuten = "新橋"; keiyu = "銀座線"; kyori = 0.9; jikan = 2 };
    { kiten = "新橋"; shuten = "虎ノ門"; keiyu = "銀座線"; kyori = 0.8; jikan = 2 };
    { kiten = "虎ノ門"; shuten = "溜池山王"; keiyu = "銀座線"; kyori = 0.6; jikan = 2 };
    { kiten = "溜池山王"; shuten = "赤坂見附"; keiyu = "銀座線"; kyori = 0.9; jikan = 2 };
    { kiten = "赤坂見附"; shuten = "青山一丁目"; keiyu = "銀座線"; kyori = 1.3; jikan = 2 };
    { kiten = "青山一丁目"; shuten = "外苑前"; keiyu = "銀座線"; kyori = 0.7; jikan = 2 };
    { kiten = "外苑前"; shuten = "表参道"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 };
    { kiten = "表参道"; shuten = "渋谷"; keiyu = "銀座線"; kyori = 1.3; jikan = 1 };
    { kiten = "渋谷"; shuten = "表参道"; keiyu = "半蔵門線"; kyori = 1.3; jikan = 2 };
    { kiten = "表参道"; shuten = "青山一丁目"; keiyu = "半蔵門線"; kyori = 1.4; jikan = 2 };
    { kiten = "青山一丁目"; shuten = "永田町"; keiyu = "半蔵門線"; kyori = 1.3; jikan = 2 };
    { kiten = "永田町"; shuten = "半蔵門"; keiyu = "半蔵門線"; kyori = 1.0; jikan = 2 };
    { kiten = "半蔵門"; shuten = "九段下"; keiyu = "半蔵門線"; kyori = 1.6; jikan = 2 };
    { kiten = "九段下"; shuten = "神保町"; keiyu = "半蔵門線"; kyori = 0.4; jikan = 1 };
    { kiten = "神保町"; shuten = "大手町"; keiyu = "半蔵門線"; kyori = 1.7; jikan = 3 };
    { kiten = "大手町"; shuten = "三越前"; keiyu = "半蔵門線"; kyori = 0.7; jikan = 1 };
    { kiten = "三越前"; shuten = "水天宮前"; keiyu = "半蔵門線"; kyori = 1.3; jikan = 2 };
    { kiten = "水天宮前"; shuten = "清澄白河"; keiyu = "半蔵門線"; kyori = 1.7; jikan = 3 };
    { kiten = "清澄白河"; shuten = "住吉"; keiyu = "半蔵門線"; kyori = 1.9; jikan = 3 };
    { kiten = "住吉"; shuten = "錦糸町"; keiyu = "半蔵門線"; kyori = 1.; jikan = 2 };
    { kiten = "錦糸町"; shuten = "押上"; keiyu = "半蔵門線"; kyori = 1.4; jikan = 2 };
    { kiten = "中目黒"; shuten = "恵比寿"; keiyu = "日比谷線"; kyori = 1.; jikan = 2 };
    { kiten = "恵比寿"; shuten = "広尾"; keiyu = "日比谷線"; kyori = 1.5; jikan = 3 };
    { kiten = "広尾"; shuten = "六本木"; keiyu = "日比谷線"; kyori = 1.7; jikan = 3 };
    { kiten = "六本木"; shuten = "神谷町"; keiyu = "日比谷線"; kyori = 1.5; jikan = 3 };
    { kiten = "神谷町"; shuten = "霞ヶ関"; keiyu = "日比谷線"; kyori = 1.3; jikan = 2 };
    { kiten = "霞ヶ関"; shuten = "日比谷"; keiyu = "日比谷線"; kyori = 1.2; jikan = 2 };
    { kiten = "日比谷"; shuten = "銀座"; keiyu = "日比谷線"; kyori = 0.4; jikan = 1 };
    { kiten = "銀座"; shuten = "東銀座"; keiyu = "日比谷線"; kyori = 0.4; jikan = 1 };
    { kiten = "東銀座"; shuten = "築地"; keiyu = "日比谷線"; kyori = 0.6; jikan = 2 };
    { kiten = "築地"; shuten = "八丁堀"; keiyu = "日比谷線"; kyori = 1.; jikan = 2 };
    { kiten = "八丁堀"; shuten = "茅場町"; keiyu = "日比谷線"; kyori = 0.5; jikan = 1 };
    { kiten = "茅場町"; shuten = "人形町"; keiyu = "日比谷線"; kyori = 0.9; jikan = 2 };
    { kiten = "人形町"; shuten = "小伝馬町"; keiyu = "日比谷線"; kyori = 0.6; jikan = 1 };
    { kiten = "小伝馬町"; shuten = "秋葉原"; keiyu = "日比谷線"; kyori = 0.9; jikan = 2 };
    { kiten = "秋葉原"; shuten = "仲御徒町"; keiyu = "日比谷線"; kyori = 1.; jikan = 1 };
    { kiten = "仲御徒町"; shuten = "上野"; keiyu = "日比谷線"; kyori = 0.5; jikan = 1 };
    { kiten = "上野"; shuten = "入谷"; keiyu = "日比谷線"; kyori = 1.2; jikan = 2 };
    { kiten = "入谷"; shuten = "三ノ輪"; keiyu = "日比谷線"; kyori = 1.2; jikan = 2 };
    { kiten = "三ノ輪"; shuten = "南千住"; keiyu = "日比谷線"; kyori = 0.8; jikan = 2 };
    { kiten = "南千住"; shuten = "北千住"; keiyu = "日比谷線"; kyori = 1.8; jikan = 3 };
    { kiten = "池袋"; shuten = "新大塚"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 3 };
    { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 };
    { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
    { kiten = "後楽園"; shuten = "本郷三丁目"; keiyu = "丸ノ内線"; kyori = 0.8; jikan = 1 };
    { kiten = "本郷三丁目"; shuten = "御茶ノ水"; keiyu = "丸ノ内線"; kyori = 0.8; jikan = 1 };
    { kiten = "御茶ノ水"; shuten = "淡路町"; keiyu = "丸ノ内線"; kyori = 0.8; jikan = 1 };
    { kiten = "淡路町"; shuten = "大手町"; keiyu = "丸ノ内線"; kyori = 0.9; jikan = 2 };
    { kiten = "大手町"; shuten = "東京"; keiyu = "丸ノ内線"; kyori = 0.6; jikan = 1 };
    { kiten = "東京"; shuten = "銀座"; keiyu = "丸ノ内線"; kyori = 1.1; jikan = 2 };
    { kiten = "銀座"; shuten = "霞ヶ関"; keiyu = "丸ノ内線"; kyori = 1.0; jikan = 2 };
    { kiten = "霞ヶ関"; shuten = "国会議事堂前"; keiyu = "丸ノ内線"; kyori = 0.7; jikan = 1 };
    {
      kiten = "国会議事堂前";
      shuten = "赤坂見附";
      keiyu = "丸ノ内線";
      kyori = 0.9;
      jikan = 2;
    };
    { kiten = "赤坂見附"; shuten = "四ツ谷"; keiyu = "丸ノ内線"; kyori = 1.3; jikan = 2 };
    { kiten = "四ツ谷"; shuten = "四谷三丁目"; keiyu = "丸ノ内線"; kyori = 1.0; jikan = 2 };
    {
      kiten = "四谷三丁目";
      shuten = "新宿御苑前";
      keiyu = "丸ノ内線";
      kyori = 0.9;
      jikan = 1;
    };
    {
      kiten = "新宿御苑前";
      shuten = "新宿三丁目";
      keiyu = "丸ノ内線";
      kyori = 0.7;
      jikan = 1;
    };
    { kiten = "新宿三丁目"; shuten = "新宿"; keiyu = "丸ノ内線"; kyori = 0.3; jikan = 1 };
    { kiten = "新宿"; shuten = "西新宿"; keiyu = "丸ノ内線"; kyori = 0.8; jikan = 1 };
    { kiten = "西新宿"; shuten = "中野坂上"; keiyu = "丸ノ内線"; kyori = 1.1; jikan = 2 };
    { kiten = "中野坂上"; shuten = "新中野"; keiyu = "丸ノ内線"; kyori = 1.1; jikan = 2 };
    { kiten = "新中野"; shuten = "東高円寺"; keiyu = "丸ノ内線"; kyori = 1.0; jikan = 1 };
    { kiten = "東高円寺"; shuten = "新高円寺"; keiyu = "丸ノ内線"; kyori = 0.9; jikan = 1 };
    { kiten = "新高円寺"; shuten = "南阿佐ヶ谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 };
    { kiten = "南阿佐ヶ谷"; shuten = "荻窪"; keiyu = "丸ノ内線"; kyori = 1.5; jikan = 2 };
    { kiten = "中野坂上"; shuten = "中野新橋"; keiyu = "丸ノ内線"; kyori = 1.3; jikan = 2 };
    {
      kiten = "中野新橋";
      shuten = "中野富士見町";
      keiyu = "丸ノ内線";
      kyori = 0.6;
      jikan = 1;
    };
    { kiten = "中野富士見町"; shuten = "方南町"; keiyu = "丸ノ内線"; kyori = 1.3; jikan = 2 };
    { kiten = "市ヶ谷"; shuten = "四ツ谷"; keiyu = "南北線"; kyori = 1.0; jikan = 2 };
    { kiten = "四ツ谷"; shuten = "永田町"; keiyu = "南北線"; kyori = 1.3; jikan = 3 };
    { kiten = "永田町"; shuten = "溜池山王"; keiyu = "南北線"; kyori = 0.9; jikan = 1 };
    { kiten = "溜池山王"; shuten = "六本木一丁目"; keiyu = "南北線"; kyori = 0.9; jikan = 2 };
    { kiten = "六本木一丁目"; shuten = "麻布十番"; keiyu = "南北線"; kyori = 1.2; jikan = 2 };
    { kiten = "麻布十番"; shuten = "白金高輪"; keiyu = "南北線"; kyori = 1.3; jikan = 2 };
    { kiten = "白金高輪"; shuten = "白金台"; keiyu = "南北線"; kyori = 1.0; jikan = 2 };
    { kiten = "白金台"; shuten = "目黒"; keiyu = "南北線"; kyori = 1.3; jikan = 2 };
    { kiten = "市ヶ谷"; shuten = "飯田橋"; keiyu = "南北線"; kyori = 1.1; jikan = 2 };
    { kiten = "飯田橋"; shuten = "後楽園"; keiyu = "南北線"; kyori = 1.4; jikan = 2 };
    { kiten = "後楽園"; shuten = "東大前"; keiyu = "南北線"; kyori = 1.3; jikan = 3 };
    { kiten = "東大前"; shuten = "本駒込"; keiyu = "南北線"; kyori = 0.9; jikan = 2 };
    { kiten = "本駒込"; shuten = "駒込"; keiyu = "南北線"; kyori = 1.4; jikan = 2 };
    { kiten = "駒込"; shuten = "西ヶ原"; keiyu = "南北線"; kyori = 1.4; jikan = 2 };
    { kiten = "西ヶ原"; shuten = "王子"; keiyu = "南北線"; kyori = 1.0; jikan = 2 };
    { kiten = "王子"; shuten = "王子神谷"; keiyu = "南北線"; kyori = 1.2; jikan = 2 };
    { kiten = "王子神谷"; shuten = "志茂"; keiyu = "南北線"; kyori = 1.6; jikan = 3 };
    { kiten = "志茂"; shuten = "赤羽岩淵"; keiyu = "南北線"; kyori = 1.1; jikan = 2 };
    { kiten = "西船橋"; shuten = "原木中山"; keiyu = "東西線"; kyori = 1.9; jikan = 3 };
    { kiten = "原木中山"; shuten = "妙典"; keiyu = "東西線"; kyori = 2.1; jikan = 2 };
    { kiten = "妙典"; shuten = "行徳"; keiyu = "東西線"; kyori = 1.3; jikan = 2 };
    { kiten = "行徳"; shuten = "南行徳"; keiyu = "東西線"; kyori = 1.5; jikan = 2 };
    { kiten = "南行徳"; shuten = "浦安"; keiyu = "東西線"; kyori = 1.2; jikan = 2 };
    { kiten = "浦安"; shuten = "葛西"; keiyu = "東西線"; kyori = 1.9; jikan = 2 };
    { kiten = "葛西"; shuten = "西葛西"; keiyu = "東西線"; kyori = 1.2; jikan = 2 };
    { kiten = "西葛西"; shuten = "南砂町"; keiyu = "東西線"; kyori = 2.7; jikan = 2 };
    { kiten = "南砂町"; shuten = "東陽町"; keiyu = "東西線"; kyori = 1.2; jikan = 2 };
    { kiten = "東陽町"; shuten = "木場"; keiyu = "東西線"; kyori = 0.9; jikan = 1 };
    { kiten = "木場"; shuten = "門前仲町"; keiyu = "東西線"; kyori = 1.1; jikan = 1 };
    { kiten = "門前仲町"; shuten = "茅場町"; keiyu = "東西線"; kyori = 1.8; jikan = 2 };
    { kiten = "茅場町"; shuten = "日本橋"; keiyu = "東西線"; kyori = 0.5; jikan = 1 };
    { kiten = "日本橋"; shuten = "大手町"; keiyu = "東西線"; kyori = 0.8; jikan = 1 };
    { kiten = "大手町"; shuten = "竹橋"; keiyu = "東西線"; kyori = 1.0; jikan = 2 };
    { kiten = "竹橋"; shuten = "九段下"; keiyu = "東西線"; kyori = 1.0; jikan = 1 };
    { kiten = "九段下"; shuten = "飯田橋"; keiyu = "東西線"; kyori = 0.7; jikan = 1 };
    { kiten = "飯田橋"; shuten = "神楽坂"; keiyu = "東西線"; kyori = 1.2; jikan = 2 };
    { kiten = "神楽坂"; shuten = "早稲田"; keiyu = "東西線"; kyori = 1.2; jikan = 2 };
    { kiten = "早稲田"; shuten = "高田馬場"; keiyu = "東西線"; kyori = 1.7; jikan = 3 };
    { kiten = "高田馬場"; shuten = "落合"; keiyu = "東西線"; kyori = 1.9; jikan = 3 };
    { kiten = "落合"; shuten = "中野"; keiyu = "東西線"; kyori = 2.0; jikan = 3 };
    { kiten = "新木場"; shuten = "辰巳"; keiyu = "有楽町線"; kyori = 1.5; jikan = 2 };
    { kiten = "辰巳"; shuten = "豊洲"; keiyu = "有楽町線"; kyori = 1.7; jikan = 2 };
    { kiten = "豊洲"; shuten = "月島"; keiyu = "有楽町線"; kyori = 1.4; jikan = 2 };
    { kiten = "月島"; shuten = "新富町"; keiyu = "有楽町線"; kyori = 1.3; jikan = 2 };
    { kiten = "新富町"; shuten = "銀座一丁目"; keiyu = "有楽町線"; kyori = 0.7; jikan = 1 };
    { kiten = "銀座一丁目"; shuten = "有楽町"; keiyu = "有楽町線"; kyori = 0.5; jikan = 1 };
    { kiten = "有楽町"; shuten = "桜田門"; keiyu = "有楽町線"; kyori = 1.0; jikan = 1 };
    { kiten = "桜田門"; shuten = "永田町"; keiyu = "有楽町線"; kyori = 0.9; jikan = 2 };
    { kiten = "永田町"; shuten = "麹町"; keiyu = "有楽町線"; kyori = 0.9; jikan = 1 };
    { kiten = "麹町"; shuten = "市ヶ谷"; keiyu = "有楽町線"; kyori = 0.9; jikan = 1 };
    { kiten = "市ヶ谷"; shuten = "飯田橋"; keiyu = "有楽町線"; kyori = 1.1; jikan = 2 };
    { kiten = "飯田橋"; shuten = "江戸川橋"; keiyu = "有楽町線"; kyori = 1.6; jikan = 3 };
    { kiten = "江戸川橋"; shuten = "護国寺"; keiyu = "有楽町線"; kyori = 1.3; jikan = 2 };
    { kiten = "護国寺"; shuten = "東池袋"; keiyu = "有楽町線"; kyori = 1.1; jikan = 2 };
    { kiten = "東池袋"; shuten = "池袋"; keiyu = "有楽町線"; kyori = 2.0; jikan = 2 };
    { kiten = "池袋"; shuten = "要町"; keiyu = "有楽町線"; kyori = 1.2; jikan = 2 };
    { kiten = "要町"; shuten = "千川"; keiyu = "有楽町線"; kyori = 1.0; jikan = 1 };
    { kiten = "千川"; shuten = "小竹向原"; keiyu = "有楽町線"; kyori = 1.0; jikan = 2 };
    { kiten = "小竹向原"; shuten = "氷川台"; keiyu = "有楽町線"; kyori = 1.5; jikan = 2 };
    { kiten = "氷川台"; shuten = "平和台"; keiyu = "有楽町線"; kyori = 1.4; jikan = 2 };
    { kiten = "平和台"; shuten = "営団赤塚"; keiyu = "有楽町線"; kyori = 1.8; jikan = 2 };
    { kiten = "営団赤塚"; shuten = "営団成増"; keiyu = "有楽町線"; kyori = 1.5; jikan = 2 };
    { kiten = "営団成増"; shuten = "和光市"; keiyu = "有楽町線"; kyori = 2.1; jikan = 3 };
  ]

(* ローマ字の駅名を表す文字列 ekimei と駅名リスト ekimei_list を受け取り、駅の漢字表記を返す *)
let rec romaji_to_kanji ekimei ekimei_lst =
  match ekimei_lst with
  | [] -> ""
  | { kanji; kana; romaji; shozoku } :: rest ->
      if romaji = ekimei then kanji else romaji_to_kanji ekimei rest

let test = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
let test = romaji_to_kanji "ikebukuro" global_ekimei_list = "池袋"
let test = romaji_to_kanji "sapporo" global_ekimei_list = ""

(* 漢字の駅名を二つ sta1 sta2 と、駅間の経路のリスト lst を受け取り、二駅間の距離を返す *)
let rec get_ekikan_kyori sta1 sta2 lst =
  match lst with
  | [] -> infinity
  | { kiten; shuten; keiyu; kyori; jikan } :: rest ->
      if (kiten, shuten) = (sta1, sta2) || (kiten, shuten) = (sta2, sta1) then
        kyori
      else get_ekikan_kyori sta1 sta2 rest

let test = get_ekikan_kyori "霞ヶ関" "国会議事堂前" global_ekikan_list = 0.7

let test = get_ekikan_kyori "霞ヶ関" "茗荷谷" global_ekikan_list = infinity
let test = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2

(* ローマ字の駅名二つ roman1 roman2 を受け取り、距離を調べ、直接つながっている場合は距離を伝える文字列を返す  *)
(* 二つの駅が直接つながっていない場合はつながっていないという意図の文字列を、入力されたローマ字の駅名が存在しない場合は駅が存在しない旨を伝える文字列を返す *)
let kyori_wo_hyoji roman1 roman2 =
  let kanji1 = romaji_to_kanji roman1 global_ekimei_list in
  let kanji2 = romaji_to_kanji roman2 global_ekimei_list in
  let noStationMsg = " という駅は存在しません" in
  if kanji1 = "" then roman1 ^ noStationMsg
  else if kanji2 = "" then roman2 ^ noStationMsg
  else
    let kyori = get_ekikan_kyori kanji1 kanji2 global_ekikan_list in
    if kyori = infinity then kanji1 ^ "と" ^ kanji2 ^ "はつながっていません"
    else kanji1 ^ "から" ^ kanji2 ^ "までは " ^ string_of_float kyori ^ " キロです"

let test1 = kyori_wo_hyoji "myougadani" "shinotsuka" = "myougadani という駅は存在しません"

let test1 = kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷から新大塚までは 1.2 キロです"

let test1 = kyori_wo_hyoji "myogadani" "ikebukuro" = "茗荷谷と池袋はつながっていません"

let test1 = kyori_wo_hyoji "tokyo" "ootemachi" = "ootemachi という駅は存在しません"

let test1 = kyori_wo_hyoji "tokyo" "otemachi" = "東京から大手町までは 0.6 キロです"

(* stationType listを受け取り、ekiType listに駅名をコピーして返す *)
let rec make_eki_list lst =
  match lst with
  | [] -> []
  | { kanji; kana; romaji; shozoku } :: rest ->
      { namae = kanji; saitan_kyori = infinity; temae_list = [] }
      :: make_eki_list rest

(* ekiType listを初期化するための関数として、ekiType listと起点となる駅名の文字列を受け取り、同名の駅のみ最短距離を0に、temae_listを起点の駅名としたリストを返す *)
let rec shokika lst eki =
  match lst with
  | [] -> []
  | ({ namae; saitan_kyori; temae_list } as first) :: rest ->
      let record =
        if namae = eki then { namae; saitan_kyori = 0.; temae_list = [ namae ] }
        else first
      in
      record :: shokika rest eki

(* 駅リストの例 *)
let eki_list =
  [
    { namae = "池袋"; saitan_kyori = infinity; temae_list = [] };
    { namae = "新大塚"; saitan_kyori = infinity; temae_list = [] };
    { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] };
    { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] };
    { namae = "本郷三丁目"; saitan_kyori = infinity; temae_list = [] };
    { namae = "御茶ノ水"; saitan_kyori = infinity; temae_list = [] };
  ]

(* テスト *)
let test1 = shokika [] "茗荷谷" = []

let test2 =
  shokika eki_list "茗荷谷"
  = [
      { namae = "池袋"; saitan_kyori = infinity; temae_list = [] };
      { namae = "新大塚"; saitan_kyori = infinity; temae_list = [] };
      { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = [ "茗荷谷" ] };
      { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] };
      { namae = "本郷三丁目"; saitan_kyori = infinity; temae_list = [] };
      { namae = "御茶ノ水"; saitan_kyori = infinity; temae_list = [] };
    ]

(* stationTypeとkanaフィールドでソートされたstationType listを受け取り、ソートを保持したままリストに加える。ただし、kanaが重複したものはリストに加えない *)
let rec kanaIns eki lst =
  match eki with
  | { kanji = kanji_e; kana = kana_l; romaji = romaji_l; shozoku = shozoku_l }
    -> (
      match lst with
      | [] -> [ eki ]
      | ({ kanji; kana; romaji; shozoku } as first) :: rest ->
          if kana_l < kana then eki :: first :: rest
          else if kana_l = kana then first :: rest
          else first :: kanaIns eki rest)

(* stationType listを受け取り、ひらがな順に整列したstationType listを返す。 *)
let rec seiretsu lst =
  match lst with [] -> [] | first :: rest -> kanaIns first (seiretsu rest)

let ekimei_list =
  [
    { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
    { kanji = "新大塚"; kana = "しんおおつか"; romaji = "shinotsuka"; shozoku = "丸ノ内線" };
    { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
    { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "副都心線" };
    {
      kanji = "本郷三丁目";
      kana = "ほんごうさんちょうめ";
      romaji = "hongosanchome";
      shozoku = "丸ノ内線";
    };
    { kanji = "御茶ノ水"; kana = "おちゃのみず"; romaji = "ochanomizu"; shozoku = "丸ノ内線" };
    {
      kanji = "本郷三丁目";
      kana = "ほんごうさんちょうめ";
      romaji = "hongosanchome";
      shozoku = "大江戸線";
    };
  ]

(* テスト *)
let test3 = seiretsu [] = []

let test4 =
  seiretsu ekimei_list
  = [
      { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "副都心線" };
      {
        kanji = "御茶ノ水";
        kana = "おちゃのみず";
        romaji = "ochanomizu";
        shozoku = "丸ノ内線";
      };
      { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
      {
        kanji = "新大塚";
        kana = "しんおおつか";
        romaji = "shinotsuka";
        shozoku = "丸ノ内線";
      };
      {
        kanji = "本郷三丁目";
        kana = "ほんごうさんちょうめ";
        romaji = "hongosanchome";
        shozoku = "大江戸線";
      };
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    ]

(* 直前に確定した駅 p: ekiType と未確定の駅 q: ekiTypeを受け取り、pとqが直接つながっているかを調べ、つながっていた場合qの最短距離と手前リストを更新。つながっていない場合はqをそのまま返す *)
let koushin1 p q =
  match p with
  | { namae = namae_p; saitan_kyori = kyori_p; temae_list = temae_p } -> (
      match q with
      | { namae = namae_q; saitan_kyori = kyori_q; temae_list = temae_q } ->
          let kyori_p_q = get_ekikan_kyori namae_p namae_q global_ekikan_list in
          if kyori_p_q = infinity then q
          else
            let new_kyori_q = kyori_p +. kyori_p_q in
            if new_kyori_q < kyori_q then
              {
                namae = namae_q;
                saitan_kyori = new_kyori_q;
                temae_list = namae_q :: temae_p;
              }
            else q)

(* 直前に確定した駅pと未確定の駅のリストvを受け取り、更新を行った後の未確定の駅リストを返す *)
(* List.mapを複数引数の関数に適用したい場合は func p q をlet f q = func p qとしてfuncの引数を確定さた関数fを定義したのち、List.map f lstする（lstはqのリスト） *)
let koushin p v =
  let f q = koushin1 p q in
  List.map f v

(* 駅の例 *)
let eki1 = { namae = "池袋"; saitan_kyori = infinity; temae_list = [] }

let eki2 = { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] }

let eki3 = { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = [ "茗荷谷" ] }

let eki4 = { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] }

(* 駅リストの例 *)
let lst = [ eki1; eki2; eki3; eki4 ]

(* テスト *)

let test1 = koushin1 eki3 eki1 = eki1
let test2 = koushin1 eki3 eki2 = eki2
let test3 = koushin1 eki3 eki3 = eki3

let test4 =
  koushin1 eki3 eki4
  = { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] }

let test5 =
  koushin1 eki2 eki1
  = { namae = "池袋"; saitan_kyori = 3.0; temae_list = [ "池袋"; "新大塚"; "茗荷谷" ] }

let test6 = koushin1 eki2 eki2 = eki2
let test7 = koushin1 eki2 eki3 = eki3
let test8 = koushin1 eki2 eki4 = eki4
let test1 = koushin eki2 [] = []

let test2 =
  koushin eki2 lst
  = [
      { namae = "池袋"; saitan_kyori = 3.0; temae_list = [ "池袋"; "新大塚"; "茗荷谷" ] };
      eki2;
      eki3;
      eki4;
    ]

let koushin p v lst =
  let koushin1 p q =
    match p with
    | { namae = namae_p; saitan_kyori = kyori_p; temae_list = temae_p } -> (
        match q with
        | { namae = namae_q; saitan_kyori = kyori_q; temae_list = temae_q } ->
            let kyori_p_q = get_ekikan_kyori namae_p namae_q lst in
            if kyori_p_q = infinity then q
            else
              let new_kyori_q = kyori_p +. kyori_p_q in
              if new_kyori_q < kyori_q then
                {
                  namae = namae_q;
                  saitan_kyori = new_kyori_q;
                  temae_list = namae_q :: temae_p;
                }
              else q)
  in
  let f q = koushin1 p q in
  List.map f v

let make_eki_list lst =
  List.map
    (fun eki ->
      match eki with
      | { kanji; kana; romaji; shozoku } ->
          { namae = kanji; saitan_kyori = infinity; temae_list = [] })
    lst

let shokika lst eki =
  List.map
    (fun elem ->
      match elem with
      | { namae; saitan_kyori; temae_list } ->
          if namae = eki then
            { namae; saitan_kyori = 0.; temae_list = [ namae ] }
          else elem)
    lst

(* make_eki_listとshokikaを合わせた関数 *)
let make_initial_eki_list namae_lst eki =
  List.map
    (fun elem ->
      match elem with
      | { kanji; kana; romaji; shozoku } ->
          if kanji = eki then
            { namae = kanji; saitan_kyori = 0.; temae_list = [ eki ] }
          else { namae = kanji; saitan_kyori = infinity; temae_list = [] })
    namae_lst

let ekimei_list =
  [
    { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
    { kanji = "新大塚"; kana = "しんおおつか"; romaji = "shinotsuka"; shozoku = "丸ノ内線" };
    { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
    {
      kanji = "本郷三丁目";
      kana = "ほんごうさんちょうめ";
      romaji = "hongosanchome";
      shozoku = "丸ノ内線";
    };
    { kanji = "御茶ノ水"; kana = "おちゃのみず"; romaji = "ochanomizu"; shozoku = "丸ノ内線" };
  ]

(* テスト *)
let test1 = make_initial_eki_list [] "茗荷谷" = []

let test2 =
  make_initial_eki_list ekimei_list "茗荷谷"
  = [
      { namae = "池袋"; saitan_kyori = infinity; temae_list = [] };
      { namae = "新大塚"; saitan_kyori = infinity; temae_list = [] };
      { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = [ "茗荷谷" ] };
      { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] };
      { namae = "本郷三丁目"; saitan_kyori = infinity; temae_list = [] };
      { namae = "御茶ノ水"; saitan_kyori = infinity; temae_list = [] };
    ]

(* ekiType listを受け取り、最短距離が最小のekiTypeと、それ以外のekiType listをタプルとして返す *)
let rec saitan_wo_bunri lst =
  match lst with
  | [] -> ({ namae = "foo"; saitan_kyori = infinity; temae_list = [] }, lst)
  | first :: [] -> (first, [])
  | ({ namae; saitan_kyori = kyori_f; temae_list } as first) :: rest ->
      let ({ saitan_kyori = kyori_r } as eki_r), lst_r = saitan_wo_bunri rest in
      if kyori_f < kyori_r then (first, eki_r :: lst_r)
      else (eki_r, first :: lst_r)

(* 駅の例 *)
let eki1 = { namae = "池袋"; saitan_kyori = infinity; temae_list = [] }

let eki2 = { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] }

let eki3 = { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = [ "茗荷谷" ] }

let eki4 = { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] }

(* 駅リストの例 *)
let lst = [ eki1; eki2; eki3; eki4 ]

(* テスト *)
let test = saitan_wo_bunri lst = (eki3, [ eki1; eki2; eki4 ])
(* let test = saitan_wo_bunri [ eki1 ] *)

(* fold_rightでかく *)
let saitan_wo_bunri lst =
  match lst with
  | [] -> ({ namae = "foo"; saitan_kyori = infinity; temae_list = [] }, [])
  | first :: rest ->
      List.fold_right
        (fun eki result ->
          match eki with
          | { saitan_kyori = kyori_e } -> (
              match result with
              | ({ saitan_kyori = kyori_r } as eki_r), list_r ->
                  if kyori_e < kyori_r then (eki, eki_r :: list_r)
                  else (eki_r, eki :: list_r)))
        rest (first, [])

let test = saitan_wo_bunri lst

(* ekiType listで未確定の駅リストと、pathType listで駅間経路のリストを受け取り、各駅について最短の距離と経路が入ったリストを返す *)
let rec dijkstra_main v ekikan =
  match v with
  | [] -> []
  | first :: rest ->
      let saitan_eki, nokori = saitan_wo_bunri v in
      let new_v = koushin saitan_eki nokori ekikan in
      saitan_eki :: dijkstra_main new_v ekikan

(* 駅の例 *)
let eki1 = { namae = "池袋"; saitan_kyori = infinity; temae_list = [] }

let eki2 = { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] }

let eki3 = { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = [ "茗荷谷" ] }

let eki4 = { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] }

(* 駅リストの例 *)
let lst = [ eki1; eki2; eki3; eki4 ]

(* テスト *)
let test1 = dijkstra_main [] global_ekikan_list = []
let test2 = dijkstra_main lst global_ekikan_list

(* 起点と終点の駅をローマ字の文字列で受け取り、終点の駅レコードを返す *)
let dijkstra kiten shuten =
  let unique_ekimei_list = seiretsu global_ekimei_list in
  let kiten_kanji = romaji_to_kanji kiten unique_ekimei_list in
  let shuten_kanji = romaji_to_kanji shuten unique_ekimei_list in
  let initial_eki_list = make_initial_eki_list unique_ekimei_list kiten_kanji in
  let result = dijkstra_main initial_eki_list global_ekikan_list in
  List.hd
    (List.filter
       (fun x -> match x with { namae } -> namae = shuten_kanji)
       result)

(* テスト *)
let test1 =
  dijkstra "shibuya" "gokokuji"
  = {
      namae = "護国寺";
      saitan_kyori = 9.8;
      temae_list =
        [ "護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; "青山一丁目"; "表参道"; "渋谷" ];
    }

let test2 =
  dijkstra "myogadani" "meguro"
  = {
      namae = "目黒";
      saitan_kyori = 12.7000000000000028;
      temae_list =
        [
          "目黒";
          "白金台";
          "白金高輪";
          "麻布十番";
          "六本木一丁目";
          "溜池山王";
          "永田町";
          "麹町";
          "市ヶ谷";
          "飯田橋";
          "後楽園";
          "茗荷谷";
        ];
    }

(* 駅名と「接続されている駅の駅名と距離の組　のリスト」を要素として持つ木構造の型 *)
type ekikan_tree_t =
  | Empty
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t

(* 目的：駅名と「接続されている駅の駅名と距離の組　のリスト」を受け取り、その駅までの距離を返す *)
let rec assoc eki lst =
  match lst with
  | [] -> infinity
  | (ekimei, kyori) :: rest -> if eki = ekimei then kyori else assoc eki rest

(* テスト *)
let test1 = assoc "後楽園" [] = infinity
let test2 = assoc "後楽園" [ ("新大塚", 1.2); ("後楽園", 1.8) ] = 1.8
let test3 = assoc "池袋" [ ("新大塚", 1.2); ("後楽園", 1.8) ] = infinity

(* 目的：ekikan_tree_t型の木と、kitenとshuten,kyoriを受け取り、kitenからshutenへの経路情報を木に追加する *)
let rec insert_ekikan_oneway tree kiten shuten kyori =
  match tree with
  | Empty -> Node (Empty, kiten, [ (shuten, kyori) ], Empty)
  | Node (t1, eki, lst, t2) ->
      if kiten < eki then
        Node (insert_ekikan_oneway t1 kiten shuten kyori, eki, lst, t2)
      else if kiten = eki then Node (t1, eki, (shuten, kyori) :: lst, t2)
      else Node (t1, eki, lst, insert_ekikan_oneway t2 kiten shuten kyori)

(* 目的、ekikan_tree_tの木と、pathTypeの値を受け取り、pathTypeの情報を追加する *)
let insert_ekikan tree path =
  match path with
  | { kiten; shuten; kyori } ->
      insert_ekikan_oneway
        (insert_ekikan_oneway tree shuten kiten kyori)
        kiten shuten kyori

(* 目的：ekikan_tree_tの木と、pathType listを受け取り、listの情報をすべて木に追加する *)
let inserts_ekikan tree lst = List.fold_left insert_ekikan tree lst

(* 駅間の例 *)
let ekikan1 =
  { kiten = "池袋"; shuten = "新大塚"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 3 }

let ekikan2 =
  { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 }

let ekikan3 =
  { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 }

(* テスト *)
let tree1 = insert_ekikan Empty ekikan1

let test1 =
  tree1
  = Node
      ( Empty,
        "新大塚",
        [ ("池袋", 1.8) ],
        Node (Empty, "池袋", [ ("新大塚", 1.8) ], Empty) )

let tree2 = insert_ekikan tree1 ekikan2

let test2 =
  tree2
  = Node
      ( Empty,
        "新大塚",
        [ ("茗荷谷", 1.2); ("池袋", 1.8) ],
        Node
          ( Empty,
            "池袋",
            [ ("新大塚", 1.8) ],
            Node (Empty, "茗荷谷", [ ("新大塚", 1.2) ], Empty) ) )

let tree3 = insert_ekikan tree2 ekikan3

let test3 =
  tree3
  = Node
      ( Node (Empty, "後楽園", [ ("茗荷谷", 1.8) ], Empty),
        "新大塚",
        [ ("茗荷谷", 1.2); ("池袋", 1.8) ],
        Node
          ( Empty,
            "池袋",
            [ ("新大塚", 1.8) ],
            Node (Empty, "茗荷谷", [ ("後楽園", 1.8); ("新大塚", 1.2) ], Empty) ) )

(* テスト *)
let test1 =
  inserts_ekikan Empty [ ekikan1; ekikan2; ekikan3 ]
  = Node
      ( Node (Empty, "後楽園", [ ("茗荷谷", 1.8) ], Empty),
        "新大塚",
        [ ("茗荷谷", 1.2); ("池袋", 1.8) ],
        Node
          ( Empty,
            "池袋",
            [ ("新大塚", 1.8) ],
            Node (Empty, "茗荷谷", [ ("後楽園", 1.8); ("新大塚", 1.2) ], Empty) ) )

(* 目的：漢字の駅名二つと、ekikan_tree_t型の木を受け取り、その２駅間の距離を返す *)
let rec get_ekikan_kyori eki1 eki2 tree =
  match tree with
  | Empty -> -1.
  | Node (t1, eki, lst, t2) ->
      if eki1 = eki then assoc eki2 lst
      else if eki2 = eki then assoc eki1 lst
      else if eki1 < eki then get_ekikan_kyori eki1 eki2 t1
      else get_ekikan_kyori eki1 eki2 t2

(* テスト *)
let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_tree = 1.2
let test2 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_tree = infinity
let test3 = get_ekikan_kyori "東京" "大手町" global_ekikan_tree = 0.6
