# Jogo da vida - Linguagens de programação - UFF - 2021.2
 Trabalho para disciplina de Linguagens de Programação
 
 Trabalho de implementação de uma versão modificada do Jogo da vida de John Conway.

 Entrada: (a) uma grade de tamanho definido pelo usuário onde cada célula possui três estados possíveis: vivo, morto e zumbi e (b) um valor natural "n" que denota a quantidade máxima de interações desejadas.

 Saída: (c) a grade resultante após "n" iterações; caso a partir de determinado ponto não haja mais alterações, deve ser informada a quantidade de passos até o sistema estabilizar.

 Regras
 * Se uma célula morta possui exatamente três células vivas adjacentes (horizontal, vertical ou diagonal), ela estará viva na próxima iteração. (reprodução)
 * Se uma célula viva possui ao menos duas células zumbis adjacente (horizontal, vertical ou diagonal), ela tornar-se-á zumbi na próxima iteração. (infecção)
 * Se uma célula viva possui menos do que duas células vivas e menos do que duas zumbis adjacentes (horizontal, vertical ou diagonal), ela estará morta na próxima iteração. (subpopulação)
 * Se uma célula viva possui mais do que três células vivas e nenhuma zumbi adjacentes (horizontal, vertical ou diagonal), ela estará morta na próxima iteração. (superpopulação)
 * Se uma célula zumbi não possui células vivas adjacentes (horizontal, vertical ou diagonal), ela estará morta na próxima iteração. (inanição)

 Restrições
 * (i) O programa deve ser desenvolvido puramente através de instruções funcionais na linguagem Haskell.
 * (ii) A entrega deve constar de (a) arquivos fonte, [(b) Makefile ou (c) roteiro completo de compilação/execução] e (d) exemplos de uso para testes.
 * (iii) O trabalho pode ser feito em grupos de até duas pessoas.
 * (iv) Em caso de duplas, deve ser incluído na entrega uma (e) descrição da atuação de cada integrante.
 * (v) Não necessariamente as notas da dupla serão iguais.
