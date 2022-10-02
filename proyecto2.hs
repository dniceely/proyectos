data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Show,Eq)

-- ejercicio 1b
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

-- ejercicio 1c
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq,Ord,Bounded,Show)  

-- ejercicio 1d
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'


-- ejercicio 3
minimoElemento :: Ord a =>[a] -> a 
minimoElemento [x] = x 
minimoElemento (x:xs) = x `min` (minimoElemento xs)

minimoElemento' :: (Bounded a, Ord a) => [a] -> a 
minimoElemento' [] = maxBound
minimoElemento' [x] = x 
minimoElemento' (x:y:xs)
                        | y < x = minimoElemento' (y:xs)
                        | x <= y = minimoElemento' (x:xs) 


-- ejercicio 4a
type Ingreso = Int
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Show,Eq)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Show,Eq)
data Persona = Decane 
             | Docente Cargo 
             | NoDocente Area 
             | Estudiante Carrera Ingreso  deriving (Show,Eq)

-- ejercicio 4b
-- Docente tiene tipo Cargo -> Persona


-- ejercicio 4c
cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc (( Docente k) : xs) c
            | c == k = 1 + cuantos_doc xs c
            | otherwise = cuantos_doc xs c
cuantos_doc ( _ : xs) c = cuantos_doc xs c

-- ejercicio 4d
esDocente :: Cargo -> Persona -> Bool
esDocente t' (Docente t) = t == t'
esDocente _ _ = False

cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' xs t = length (filter (esDocente t) xs) 

-- ejercicio 5
data Alteracion = Bemol | Sostenido | Natural deriving (Eq,Ord)
data NotaMusical = Nota  NotaBasica Alteracion 

sonido :: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12

alter :: NotaBasica -> Alteracion -> Int
alter x Sostenido = sonido x + 1
alter x Bemol = sonido x - 1
alter x Natural = sonido x 


sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota x y) = alter x y

instance (Eq NotaMusical) where
    nm1 == nm2 = sonidoCromatico nm1 == sonidoCromatico nm2

instance (Ord NotaMusical) where
    nm1 <= nm2 = sonidoCromatico nm1 <= sonidoCromatico nm2    

-- ejercicio 6
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento xs = Just ((xs)!!0)

-- ejercicio 7
data Cola = VaciaC | Encolada Persona Cola

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada p c) = Just c

encolar :: Persona -> Cola -> Cola
encolar p VaciaC = Encolada p VaciaC
encolar p (Encolada p' c) = Encolada p' (encolar p c) 

busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC c = Nothing
busca (Encolada p q) c 
                            | p == Docente c = Just (p)
                            | otherwise = busca q c 


-- ejercicio 8
--type GuiaTelefonica = ListaAsoc String Int

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)

la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b xs) = 1 + la_long xs

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia la2 = la2
la_concat (Nodo a b la1) la2 = Nodo a b (la_concat la1 la2)

la_agregar :: ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar la a b = Nodo a b la

la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo a b la) = (a, b) : la_pares la

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia a = Nothing
la_busca (Nodo a b la) a'
                        | a == a' = Just b 
                        | otherwise = la_busca la a' 

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar a Vacia = Vacia
la_borrar a' (Nodo a b la)
                         | a' == a = la_borrar a' la 
                         | otherwise = Nodo a b (la_borrar a' la)

-- ejercicio 9
data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)
