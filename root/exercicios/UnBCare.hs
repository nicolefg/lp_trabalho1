module Root.Exercicios.UnBCare where

import Root.Modelo.ModeloDados

{-
 *** Aluno: Nicole Ferreira Gomes
 *** Matricula: 221030858
 

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med quant [] = [(med, quant)]
comprarMedicamento med quant ((m, q):xs)
    | not (med `elem` [m | (m, _) <- ((m, q):xs)]) = (med, quant) : (m, q) : xs
    | med == m = (med, quant + q) : xs
    | otherwise = (m, q) : comprarMedicamento med quant xs

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing
tomarMedicamento med ((m, q):xs)
    | med == m && q > 1 = Just ((m, q - 1) : xs)
    | med == m && q == 1 = Just xs
    | otherwise = case tomarMedicamento med xs of
        Nothing -> Nothing
        Just xs' -> Just ((m, q) : xs')

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento med [] = 0
consultarMedicamento med ((m, q):xs)
    | med == m  = q
    | otherwise = consultarMedicamento med xs

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [e | e <- xs, e <= x] ++ [x] ++ quickSort [e | e <- xs, e > x]

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos r = quickSort [(med, length h) | (med, h) <- r]

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

sorted :: Ord a => [[a]] -> Bool
sorted [] = True
sorted (x:xs)
   | quickSort x == x && noDouble (quickSort x) = True && sorted xs
   | otherwise = False

noDouble :: (Eq a) => [a] -> Bool
noDouble [] = True
noDouble [a] = True
noDouble (x:xb:xs)
   | x /= xb = True && noDouble (xb:xs)
   | otherwise = False

receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido r =
    let meds= [med | (med, h) <- r]
        hs = [h | (med, h) <- r]
    in noDouble meds && sorted hs && (meds == quickSort meds)

planoValido :: PlanoMedicamento -> Bool
planoValido [] = True
planoValido p =
    let meds= [med | (med, h) <- p]
        hs = [h | (med, h) <- p]
    in noDouble meds && sorted hs && (meds == quickSort meds)

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

horarios :: Plantao -> Bool
horarios [_] = True
horarios ((h1, _) : (h2, c2) : xs)
  | h1 < h2 = horarios ((h2, c2) : xs)
  | otherwise = False

cuidados :: Cuidado -> Cuidado -> Bool
cuidados (Comprar med1 _) (Comprar med2 _)
  | med1 /= med2 = True
  | otherwise = False
cuidados (Comprar med1 _) (Medicar med2)
  | med1 /= med2 = True
  | otherwise = False
cuidados (Medicar med1) (Comprar med2 _)
  | med1 /= med2 = True
  | otherwise = False
cuidados (Medicar med1) (Medicar med2)
  | med1 < med2 = True
  | otherwise = False

cuidadoPorHorario :: Cuidado -> [Cuidado] -> Bool
cuidadoPorHorario _ [] = True
cuidadoPorHorario c1 (c2 : xs)
  | cuidados c1 c2 = cuidadoPorHorario c1 xs
  | otherwise = False

porHorario :: [Cuidado] -> Bool
porHorario [_] = True
porHorario (c1 : xs) = cuidadoPorHorario c1 xs

plantaoValido :: Plantao -> Bool
plantaoValido [] = True
plantaoValido plantao@((_, c) : xs)
  | horarios plantao && porHorario c = plantaoValido xs
  | otherwise = False

{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

prescricaoParaPlano :: Prescricao -> PlanoMedicamento
prescricaoParaPlano (_, []) = []
prescricaoParaPlano (med, h1 : xs) = (h1, [med]) : prescricaoParaPlano (med, xs)

juntaMed :: [Medicamento] -> [Medicamento] -> [Medicamento]
juntaMed [] [] = []
juntaMed meds [] = meds
juntaMed [] meds = meds
juntaMed (med1 : xs1) (med2 : xs2)
  | med1 == med2 = med1 : juntaMed xs1 xs2
  | med1 < med2 = med1 : juntaMed xs1 (med2 : xs2)
  | med2 < med1 = med2 : juntaMed xs2 (med1 : xs1)

juntaPlanos :: PlanoMedicamento -> PlanoMedicamento -> PlanoMedicamento
juntaPlanos [] [] = []
juntaPlanos p [] = p
juntaPlanos [] p = p
juntaPlanos ((h1, meds1) : xs1) ((h2, meds2) : xs2)
  | h1 < h2 = (h1, meds1) : juntaPlanos xs1 ((h2, meds2) : xs2)
  | h2 < h1 = (h2, meds2) : juntaPlanos xs2 ((h1, meds1) : xs1)
  | h1 == h2 = (h1, juntaMed meds1 meds2) : juntaPlanos xs1 xs2

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario (prescricao : xs) = juntaPlanos (prescricaoParaPlano prescricao) (geraPlanoReceituario xs)

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

planoParaReceita :: Horario -> [Medicamento] -> Receituario
planoParaReceita _ [] = []
planoParaReceita h (med1 : xs) = (med1, [h]) : planoParaReceita h xs

juntaHora :: [Horario] -> [Horario] -> [Horario]
juntaHora [] [] = []
juntaHora hs [] = hs
juntaHora [] hs = hs
juntaHora (h1 : xs1) (h2 : xs2)
  | h1 == h2 = h1 : juntaHora xs1 xs2
  | h1 < h2 = h1 : juntaHora xs1 (h2 : xs2)
  | h2 < h1 = h2 : juntaHora xs2 (h1 : xs1)

juntaReceita :: Receituario -> Receituario -> Receituario
juntaReceita [] [] = []
juntaReceita r [] = r
juntaReceita [] r = r
juntaReceita ((med1, hs1) : xs1) ((med2, hs2) : xs2)
  | med1 < med2 = (med1, hs1) : juntaReceita xs1 ((med2, hs2) : xs2)
  | med2 < med1 = (med2, hs2) : juntaReceita xs2 ((med1, hs1) : xs1)
  | med1 == med2 = (med1, juntaHora hs1 hs2) : juntaReceita xs1 xs2

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano [] = []
geraReceituarioPlano ((h, meds) : xs) = juntaReceita (planoParaReceita h meds) (geraReceituarioPlano xs)


{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

medicamento :: [(a, b)] -> [b]
medicamento plantao = map snd plantao

medicar :: Cuidado -> Bool
medicar (Medicar _) = True
medicar _ = False
comprar :: Cuidado -> Bool
comprar (Comprar _ _) = True
comprar _ = False

medLista :: [(a, [Cuidado])] -> [[Cuidado]]
medLista plantao = map (filter medicar) (medicamento plantao)

semMedicar :: [(a, [Cuidado])] -> [[Medicamento]]
semMedicar plantao = map (map (\(Medicar m) -> m)) (medLista plantao)

compraLista :: [(a, [Cuidado])] -> [[Cuidado]]
compraLista plantao = map (filter comprar) (medicamento plantao)

estoqueLista :: [(a, [Cuidado])] -> [(Medicamento, Quantidade)]
estoqueLista plantao = concat (map (map (\(Comprar med quant) -> (med,quant))) (compraLista plantao))

quantLista :: (Medicamento, Quantidade) -> EstoqueMedicamentos -> EstoqueMedicamentos
quantLista (med,quant) est = comprarMedicamento med quant est

tomarMedicamento2 :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
tomarMedicamento2 _ [] = []
tomarMedicamento2 med ((m, q):xs)
    | med == m && q > 1 = (m, q - 1) : xs
    | med == m && q == 1 = xs
    | otherwise = let xs' = tomarMedicamento2 med xs in (m, q) : xs'

atualiza :: [(a, [Cuidado])] -> EstoqueMedicamentos -> EstoqueMedicamentos
atualiza p est  = aux(tomarMedicamento2) (concat (semMedicar p)) est
    where aux :: (x -> y -> y) -> [x] -> y -> y
          aux _ [] est = est
          aux f1 (a:as) est = (f1 a . (aux f1 as)) est

compra :: [(a, [Cuidado])] -> EstoqueMedicamentos -> EstoqueMedicamentos
compra p est  = aux2 (quantLista) (estoqueLista p) est
    where aux2 :: ((x, y1) -> y2 -> y2) -> [(x, y1)] -> y2 -> y2
          aux2 _ [] est = est
          aux2 f1 ((a,b):as) est = (f1 (a,b) . (aux2 f1 as)) est

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao p est
  | length [y | (x,y) <- aux3 p est, y < 1] == 0 = Just (aux3 p est)
  | otherwise = Nothing     
      where aux3 :: Plantao -> EstoqueMedicamentos -> EstoqueMedicamentos
            aux3 p est = ((atualiza p) . (compra p)) est

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoque = case executaPlantao plantao estoque of
  Just _  -> True
  Nothing -> False


{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

obterHora :: PlanoMedicamento -> Horario
obterHora ((h,_):_) = h - 1

obterMed :: PlanoMedicamento -> EstoqueMedicamentos
obterMed p = acumulaMed p []
  where
    acumulaMed [] est = est
    acumulaMed ((_, meds):xs) est = acumulaMed xs (adicionaMed meds est)
    adicionaMed [] est = est
    adicionaMed (m:ms) est = adicionaMed ms (incrementaMed m est)
    incrementaMed m [] = [(m, 1)]
    incrementaMed m ((med, q):xs)
        | med == m  = (med, q + 1) : xs
        | otherwise = (med, q) : incrementaMed m xs

planoParaPlantao :: PlanoMedicamento -> Plantao
planoParaPlantao = map (\(h, meds) -> (h, map Medicar meds))

quantidade :: (Medicamento, Quantidade) -> EstoqueMedicamentos -> (Medicamento, Quantidade)
quantidade (med, quant) est = let quant2 = consultarMedicamento med est
  in if quant2 < quant
     then (med, quant - quant2)
     else (med, 0)

difEst :: EstoqueMedicamentos -> EstoqueMedicamentos -> EstoqueMedicamentos
difEst est1 est2 = filter (\(med, quant) -> quant /= 0) $ map (`quantidade` est2) est1

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto p est = let
    estoque = obterMed p
    dif = difEst estoque est
    compras = map (uncurry Comprar) dif
    h = obterHora p
    plantao = (h, compras)
  in
    plantao : planoParaPlantao p
