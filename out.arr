#lang pyret


O000O = 8
OO00O = 8
O0O0O = 0
OOO0O = 8
O00OO = 9
data O0O:   | OOO(O00O :: Number) end 
fun OO0O(O0OO :: O0O) -> Number:   cases (O0O) O0OO:     | OOO(OOOO) =>         if 8 == 3:           0
         else if OOOO == O000O:                     OO00O
        else:           O0O0O
        end 
  end
end
print(OO0O(OOO(OOO0O)))
print(OO0O(OOO(O00OO)))
