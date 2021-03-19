

--Ex. 1
duplicar::Int->Int
duplicar x = 2*x

quadriplica::Int->Int
quadriplica x = 2*duplicar x


hipotenusa::Double->Double->Double
hipotenusa co ca = sqrt(co^2 + ca^2) 

distancia::Double->Double->Double->Double->Double
distancia x1 y1 x2 y2  =  sqrt((x2 - x1)^2 + (y2 - y1)^2)



--Ex.3
converte::Float->(Float,Float,Float)
converte x = (x,x*3.96,x*4.45)

--Ex.4
bissexto::Int->Bool
bissexto x = if(mod x 4 == 0 && mod x 100 /= 0 || mod x 400 == 0) then True
                                                                  else False

--Ex.5
type Data = (Int, Int, Int)
bissexto2::Data->Bool
bissexto2 (x,y,z)
  | x<=29 && x>0 && y==2 &&(bissexto z == True) = True
  | x<=31 && x>0 &&(y==1 || y==3 || y==5 || y==7 || y==8 || y==10 || y==12) &&(bissexto z == True) = True
  | x<=30 && x>0 &&( y==4 || y==6 || y==9 || y==11) &&(bissexto z == True) = True
  | otherwise    = False

--Ex.6
valida::Data->Bool
valida (x,y,z)
  | x<=29 && x>0 && y==2 &&(bissexto z == True) = True
  | x<=28 && x>0 && y==2 = True
  | x<=31 && x>0 &&(y==1 || y==3 || y==5 || y==7 || y==8 || y==10 || y==12) = True
  | x<=30 && x>0 &&( y==4 || y==6 || y==9 || y==11) = True
  | otherwise    = False


--Ex.7
precede::Data->Data->Bool
precede (x1,y1,z1) (x2,y2,z2) = if(valida (x1,y1,z1) == True && valida (x2,y2,z2) ==True &&x1==(x2-1)&& y2==y1 && z2==z1) then True
                               else False

--Ex.8
type Livro = (String,String,String,String,Int)
type Aluno = (String, String, String, Int)
type Emprestimo = (String, String, Data, Data, String)

--Ex.9
e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")

verificaEmp::Emprestimo->Data->Bool
verificaEmp (_,_,(x1,y1,z1),(x2,y2,z2),_) (x3,y3,z3)
  |(x3 > x1) && (y3 == y1) && (z3 == z1) && (valida (x3,y3,z3) == True) && 
	(valida (x1,y1,z1) == True) && (x2 > x3) && (y2 == y3) && (z2 == z3) && (valida (x3,y3,z3) == True) && 
	(valida (x2,y2,z2) == True) = True
	| (y3 > y1) && (z3 == z1) && (valida (x1,y1,z1) == True) && 
	(valida (x3,y3,z3) == True) && (y2 > y3) && (z2 == z3) && (valida (x2,y2,z2) == True) && 
	(valida (x3,y3,z3) == True) = True
	| (z3 > z1) && (valida (x1,y1,z1) == True) && (valida (x3,y3,z3) == True) &&
	(z3 > z2) && (valida (x2,y2,z2) == True) = True
	| otherwise = False