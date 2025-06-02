
--TAD
data Organizador = Org (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

{-
    INV.REP: Sea Org mcsp mpsc  
    * todos las claves que aparecen en mcsp tambien aparecen en los valores del set de mpsc y todas las claves que aparecen en mpsc tambien aparecen en los valores del set de mcsp 
    * no existe conjuntos vacios como valores de mcsp y mpsc
    


-}

nuevo :: Organizador
-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)
nuevo = Org emptyM emptyM

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
-- Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
-- de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
-- no está vacío.
-- Eficiencia: no hay ninguna garantía de eficiencia.
agregarPrograma (Org mcsp mpsc) c sp = Org (assocM c sp mcsp) ()

todosLosProgramas :: Organizador -> [Checksum]
-- Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
autoresDe :: Organizador -> Checksum -> Set Persona
-- Propósito: denota el conjunto de autores que aparecen en un programa determinado.
-- Precondición: el Checksum debe corresponder a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
programasDe :: Organizador -> Persona -> Set Checksum
-- Propósito: denota el conjunto de programas en los que participó una determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
-- Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
-- programas del organizador, y C la cantidad total de programas.
nroProgramasDePersona :: Organizador -> Persona -> Int
-- Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.






--USUARIO
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas programaron juntas.
programasEnComun p1 p2 o = if programaronJuntas o p1 p2 
                           then intersection (programasDe o p1) (programasDe o p2) 
                           else emptyS

esUnGranHacker :: Organizador -> Persona -> Bool
-- Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador
esUnGranHacker o p = esAutorDeTodos o (todosLosProgramas o) p 

esAutorDeTodos :: Organizador -> [Checksum] -> Persona -> Bool 
esAutorDeTodos o [] p = True
esAutorDeTodos o (c:cs) p = esAutor o c p && esAutorDeTodos o cs p 

esAutor :: Organizador -> Checksum -> Persona -> Bool 
esAutor o c p = belongs p (autoresDe o c)



