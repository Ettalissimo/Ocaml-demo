(* Exercice a rendre *)
(* TO DO : contrat *)
(*Contrat
  fonction PGCD: Plus Grand Commun Diviseur
  profil: int -> int -> int
  parametre : 2 nombres entiers
  precondition :
  Resultat : un entier qui represente le plus grand diviseur commun
*)

let rec pgcd a b =
  if a < 0 || b < 0 then
    pgcd (-a) (-b)
  else 
    if a = 0 || b = 0 then
      1
    else if a = b then
      a
    else if a = 0 then
      b
    else if b = 0 then
      a
    else
      if a > b then
        pgcd (a - b) b
      else
        pgcd a (b - a)


let%test _ = pgcd 0 0 = 1
let%test _ = pgcd 56 98 = 14
let%test _ = pgcd 20 30 = 10
let%test _ = pgcd 100 10 = 10
let%test _ = pgcd 0 5 = 5
let%test _ = pgcd 5 0 = 5
let%test _ = pgcd (-56) (-98) = 14
let%test _ = pgcd 13 7 = 1
let%test _ = pgcd (-13) 7 = 1

