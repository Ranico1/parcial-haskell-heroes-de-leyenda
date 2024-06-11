

data Heroe = Heroe {
epiteto :: String,
reconocimiento :: Float,
artefactos :: [Artefacto],
tareas :: [Tarea]
}
-- MODELAR HERACLES -- 
heracles = Heroe {
epiteto = "guardian del olimpo",
reconocimiento = 700,
artefactos = [pistola, relampagoDeZeus],
tareas = [matarUnaBestia leonDeNemea]
}


data Artefacto = Artefacto {
nombre :: String, 
rareza :: Float
}

-- MODELAR PISTOLA --
pistola = Artefacto {
nombre = "pistola",
rareza = 1000
}
-- MODELAR LANZADELOLIMPO --
lanzaDelOlimpo = Artefacto {
nombre = "lanza del olimpo",
rareza = 100
}

-- MODELAR XIPHOS --
xiphos = Artefacto {
nombre = "xiphos",
rareza = 50
}

-- MODELAR RELAMPAGO --

relampagoDeZeus = Artefacto {
nombre = "relampago de zeus",
rareza = 500
}


-- PUNTO 2 --

pasarAlaHistoria :: Heroe -> Heroe 
pasarAlaHistoria unHeroe 
    | reconocimiento unHeroe > 1000 = cambiarNombreA "el Mitico" unHeroe
    | reconocimiento unHeroe > 500  = (anadirArtefacto lanzaDelOlimpo.cambiarNombreA "el magnifico") unHeroe
    | reconocimiento unHeroe > 100  = (anadirArtefacto xiphos.cambiarNombreA "hopilita") unHeroe 
    | otherwise                     = unHeroe 

cambiarNombreA :: String -> Heroe -> Heroe
cambiarNombreA epitetoNuevo unHeroe = unHeroe {epiteto = epitetoNuevo}

anadirArtefacto :: Artefacto -> Heroe -> Heroe 
anadirArtefacto unArtefacto = mapArtefactos (unArtefacto :)

mapArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe 
mapArtefactos f unHeroe  = unHeroe {artefactos = f $ artefactos unHeroe}


-- Tareas de los heroes -- 
type Tarea = Heroe -> Heroe 

-- MODELAR TAREA MATAR AL LEON --
leonDeNemea = Bestia {
nombreBestia = "Leon de Nemea",
debilidad = epitetoLargo 
}

epitetoLargo :: Heroe -> Bool 
epitetoLargo = (>=20).length.epiteto

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto unArtefacto = anadirArtefacto unArtefacto.ganarReconocimiento (rareza unArtefacto) 

ganarReconocimiento :: Float -> Heroe -> Heroe 
ganarReconocimiento cantReconocimiento = mapReconocimiento (+ cantReconocimiento)

mapReconocimiento :: (Float -> Float) -> Heroe -> Heroe 
mapReconocimiento f unHeroe = unHeroe {reconocimiento = f $ reconocimiento unHeroe}


escalarAlOlimpo :: Tarea
escalarAlOlimpo  = anadirArtefacto relampagoDeZeus.desecharPocosArtefactos.aumentarRarezaArtefactos.ganarReconocimiento 500

aumentarRarezaArtefactos :: Heroe -> Heroe 
aumentarRarezaArtefactos  = mapListaArtefactos (*3) 

mapListaArtefactos :: (Float -> Float ) -> Heroe -> Heroe 
mapListaArtefactos f unHeroe = unHeroe {artefactos = map (mapRarezaIndividual f) $ artefactos unHeroe }

mapRarezaIndividual :: (Float -> Float) -> Artefacto -> Artefacto 
mapRarezaIndividual f unArtefacto = unArtefacto {rareza = f $ rareza unArtefacto}


desecharPocosArtefactos :: Heroe -> Heroe 
desecharPocosArtefactos unHeroe = seleccionarArtefactosConCantidad unHeroe 

seleccionarArtefactosConCantidad :: Heroe -> Heroe 
seleccionarArtefactosConCantidad unheroe = mapArtefactos (filter muchaRareza) unheroe

muchaRareza :: Artefacto -> Bool 
muchaRareza = (>1000).rareza

ayudarAcruzarLaCalle :: Int -> Tarea
ayudarAcruzarLaCalle cantCuadras = cambiarNombreA (nivelDeGroso cantCuadras)  

nivelDeGroso :: Int -> String
nivelDeGroso cantCuadras = "gros" ++ replicate cantCuadras 'o'


-- MODELAR BESTIA --
data Bestia = Bestia {
nombreBestia :: String,
debilidad :: Debilidad 
}

type Debilidad = Heroe -> Bool 

matarUnaBestia :: Bestia -> Tarea 
matarUnaBestia unaBestia unHeroe 
    | aprovecharDebilidadDe unaBestia unHeroe = cambiarNombreA (" el asesino de " ++ nombreBestia unaBestia) unHeroe
    | otherwise = (cambiarNombreA "el cobarde".mapArtefactos tail) unHeroe
    where aprovecharDebilidadDe = debilidad 

-- PUNTO 6 -- 

hacerTarea :: Tarea -> Heroe -> Heroe 
hacerTarea unaTarea = agregarTarea unaTarea.unaTarea

agregarTarea :: Tarea -> Heroe -> Heroe 
agregarTarea unaTarea = mapTareas (unaTarea:) 

mapTareas :: ([Tarea] -> [Tarea]) -> Heroe -> Heroe 
mapTareas f unHeroe = unHeroe {tareas = f $ tareas unHeroe}


-- PUNTO 7 --
presumirLogros :: Heroe -> Heroe -> (Heroe, Heroe)
presumirLogros heroe1 heroe2 
    | ganadorContiendaEntre heroe1 heroe2  = (heroe1, heroe2)
    | ganadorContiendaEntre heroe2 heroe1  = (heroe2, heroe1)
    | otherwise = presumirLogros (hacerTareas (tareas heroe2) heroe1) (hacerTareas (tareas heroe1) heroe2)

ganadorContiendaEntre :: Heroe -> Heroe -> Bool 
ganadorContiendaEntre heroe1 heroe2 = tieneMayorReconocimiento heroe1 heroe2 || comparacionRarezas heroe1 heroe2 

tieneMayorReconocimiento :: Heroe -> Heroe -> Bool
tieneMayorReconocimiento unHeroe otroHeroe = reconocimiento unHeroe > reconocimiento otroHeroe

comparacionRarezas :: Heroe -> Heroe -> Bool 
comparacionRarezas  unHeroe otroHeroe = sumatoriaRarezas (artefactos unHeroe) > sumatoriaRarezas (artefactos unHeroe)

sumatoriaRarezas :: [Artefacto]  -> Float 
sumatoriaRarezas = sum.map rareza

hacerTareas :: [Tarea] -> Heroe -> Heroe 
hacerTareas listaDeTareas unHeroe = foldl (\x f -> f x ) unHeroe listaDeTareas


-- quedan comparando infinitamente porque al aplicarle una lista vacia (tareas) va a seguir comparando los mismos heroes
-- labor es el hacer tareas 




