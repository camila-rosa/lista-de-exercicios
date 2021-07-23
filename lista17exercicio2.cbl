      $set sourceformat"free"

      *>divisão de identificação do programa
       identification division.
       program-id. "lista17exercicio2".
       author. "Camila da Rosa Hostin".
       installation. "PC".
       date-written. 28/07/2020.
       date-compiled. 28/07/2020.

      *>divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----declaração dos recursos externos
       input-output section.
       file-control.

           select arqEstadoCapital assign to 'arqEstadoCapital.txt'
           organization is line sequential
           access mode is sequential
           lock mode is automatic
           file status is ws-fs-arqEstadoCapital.

       i-o-control.

      *>declaração de variáveis
       data division.

      *>----variaveis de arquivos
       file section.

       fd arqEstadoCapital.
       01 fd-arqEstadoCapital.
          05 fd-estado                             pic  x(25).
          05 fd-capital                            pic  x(25).

      *>----variaveis de trabalho
       working-storage section.

       77 ws-fs-arqEstadoCapital                   pic  9(02).

       01  ws-estados occurs 27.
           05 ws-estado                            pic  x(25).
           05 ws-capital                           pic  x(25).

       01 ws-jogadores occurs 4.
          05 ws-nome-jog                           pic  x(25).
          05 ws-pontos                             pic  9(02) value zero.

       01 ws-jogadores-aux.
          05 ws-nome-jog-aux                       pic  x(25).
          05 ws-pontos-aux                         pic  9(02) value zero.

       01 ws-indices.
          05 ws-ind-est                            pic  9(02).
          05 ws-ind-jog                            pic  9(01).

       01 ws-tela-menu.
          05 ws-cadastro-jogadores                 pic  x(01).
          05 ws-jogar                              pic  x(01).


       01 ws-tela-jogo.
          05 ws-capital-jog                        pic  x(25).
          05 ws-estado-sorteado                    pic  x(25).
          05 ws-pontos-jogador                     pic  9(02).


       01 ws-uso-comum.
          05 ws-sair                               pic  x(01).
          05 ws-msn                                pic  x(50).
          05 ws-msn-erro.
              10 ws-msn-erro-offset                pic  x(04).
              10 filler                            pic  x(01) value '-'.
              10 ws-msn-erro-cod                   pic  x(1).
              10 filler                            pic  x(02) value '-'.
              10 ws-msn-erro-text                  pic  x(42).

          05 ws-nome-jogador                       pic  x(25).

       01 ws-sorteio.
          05  ws-semente                           pic  9(08).
          05  ws-num_random                        pic  9(01)v9(07).


       01 ws-controle                              pic  x(01).
          88  ws-trocou                             value "1".
          88  ws-nao_trocou                         value "5".



      *>----variaveis para comunicação entre programas
       linkage section.


      *>----declaração de tela
       screen section.
       01  sc-tela-menu.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Tela Principal                                   ".
           05 line 03 col 01 value "      MENU                                                                       ".
           05 line 04 col 01 value "        [ ]Cadastro de Jogadores                                                 ".
           05 line 05 col 01 value "        [ ]Jogar                                                                 ".


           05 sc-sair-menu            line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-cadastro-jogadores   line 04  col 10 pic x(01)
           using ws-cadastro-jogadores foreground-color 15.

           05 sc-jogar                line 05  col 10 pic x(01)
           using ws-jogar foreground-color 15.

       01  sc-tela-cad-jogador.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Cadastro de Jogadores                            ".
           05 line 03 col 01 value "                                                                                 ".
           05 line 04 col 01 value "      Jogador  :                                                                 ".
           05 line 22 col 01 value "              [__________________________________________________]               ".


           05 sc-sair-cad-jog            line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-nome-jog-cad-jog        line 04  col 17 pic x(25)
           using ws-nome-jogador foreground-color 12.

           05 sc-msn-cad-jog             line 22  col 16 pic x(50)
           from ws-msn  foreground-color 12.

       01  sc-tela-jogar.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                           Quiz Estados Brasileiros                              ".
           05 line 03 col 01 value "                                                                                 ".
           05 line 04 col 01 value "      Jogador  :                                   Pontos Acumulados:            ".
           05 line 06 col 01 value "      Qual e a capital do estado:                                                ".
           05 line 07 col 01 value "      Resposta :                                                                 ".


           05 line 22 col 01 value "              [__________________________________________________]               ".


           05 sc-sair-jog                line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-nome-jog                line 04  col 17 pic x(25)
           from ws-nome-jogador foreground-color 12.

           05 sc-pontos-jog              line 04  col 71 pic 9(02)
           from ws-pontos-jogador foreground-color 12.

           05 sc-estado-sorteado-jog     line 06  col 34 pic x(25)
           from ws-estado-sorteado foreground-color 12.


           05 sc-resposta-jog            line 07  col 17 pic x(25)
           using ws-capital-jog  foreground-color 12.


           05 sc-msn-jog                 line 22  col 16 pic x(50)
           from ws-msn  foreground-color 12.


       01  sc-tela-relatorio.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Resultados finais                                ".
           05 line 03 col 01 value "                                                                                 ".
           05 line 04 col 01 value "  Quarto colocado  :                                        Pontos:              ".
           05 line 05 col 01 value "  Terceiro colocado:                                        Pontos:              ".
           05 line 06 col 01 value "  Segundo colocado :                                        Pontos:              ".
           05 line 07 col 01 value "  Vencedor         :                                        Pontos:              ".
           05 line 22 col 01 value "              [__________________________________________________]               ".


           05 sc-sair-rel                line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-nome-jog4-rel           line 04  col 21 pic x(25)
           from ws-nome-jog(4) foreground-color 12.
           05 sc-pontos-jog4-rel         line 04  col 68 pic 9(02)
           from ws-pontos(4) foreground-color 12.

           05 sc-nome-jog3-rel           line 05  col 21 pic x(25)
           from ws-nome-jog(3) foreground-color 12.
           05 sc-pontos-jog3-rel         line 05  col 68 pic 9(02)
           from ws-pontos(3) foreground-color 12.

           05 sc-nome-jog2-rel           line 06  col 21 pic x(25)
           from ws-nome-jog(2) foreground-color 12.
           05 sc-pontos-jog2-rel         line 06  col 68 pic 9(02)
           from ws-pontos(2) foreground-color 12.

           05 sc-nome-jog1-rel           line 07  col 21 pic x(25)
           from ws-nome-jog(1) foreground-color 12.
           05 sc-pontos-jog1-rel         line 07  col 68 pic 9(02)
           from ws-pontos(1) foreground-color 12.

           05 sc-msn-rel                 line 22  col 16 pic x(50)
           from ws-msn  foreground-color 12.

      *>declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  procedimentos de inicialização
      *>------------------------------------------------------------------------
       inicializa section.

      *>   open input abre o arquivo para leitura
           open input arqEstadoCapital.
      *>   tratamento de erro - file status diferente de 0, erro ao abrir arquivo
           if ws-fs-arqEstadoCapital <> 0 then
               move 1 to ws-msn-erro-offset
               move ws-fs-arqEstadoCapital to ws-msn-erro-cod
               move 'Erro ao Abrir Arquivo arqEstadoCapital' to ws-msn-erro-text
               perform finaliza-anormal
           end-if

      *>   executa variando o índice de temperatura até o índice ser maior que 27
           perform varying ws-ind-est from 1 by 1 until ws-fs-arqEstadoCapital = 10
                                                                 or ws-ind-est > 27

      *>       lê o arquivo de estados
               read arqEstadoCapital into ws-estados(ws-ind-est)

      *>       tratamento de erro - file status diferente de 0 e 10
               if ws-fs-arqEstadoCapital <> 0
               and ws-fs-arqEstadoCapital <> 10  then
                   move 2 to ws-msn-erro-offset
                   move ws-fs-arqEstadoCapital to ws-msn-erro-cod
                   move 'Erro ao Ler Arquivo arqEstadoCapital' to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

           end-perform

      *>   fechar arquivo
           close arqEstadoCapital.
      *>   tratamento de erro  - file status diferente de 0
           if ws-fs-arqEstadoCapital <> 0 then
               move 3 to ws-msn-erro-offset
               move ws-fs-arqEstadoCapital to ws-msn-erro-cod
               move 'Erro ao Fechar Arquivo arqEstadoCapital' to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  processamento principal
      *>------------------------------------------------------------------------
       processamento section.

      *>    menu do sistema
           perform until ws-sair = "X"
                      or ws-sair = "x"

               move space  to ws-cadastro-jogadores
               move space  to ws-jogar
               move space  to ws-sair

               display sc-tela-menu
               accept sc-tela-menu

               if  ws-cadastro-jogadores  = "X"
               or  ws-cadastro-jogadores  = "x"  then
                    perform cadastrar-jogadores
               end-if

               if  ws-jogar = "X"
               or  ws-jogar = "x" then
                    perform jogar
               end-if

           end-perform
      *>   chamar impressao de relatorio
           perform relatorio-final


           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  cadastro de jogadores, sao admitidos até 4 jogadores
      *>------------------------------------------------------------------------
       cadastrar-jogadores section.

           perform until ws-sair = "V"
                      or ws-sair = "v"

               move space  to ws-nome-jogador

               display sc-tela-cad-jogador
               accept sc-tela-cad-jogador

               move space     to   ws-msn

      *>       consistindo a digitação do User, nomes = spaces  são ignorados
               if ws-nome-jogador <> space then
                   perform descobrir-prox-ind-jog

      *>           consistencia da quantidade de jogadores para evitar estouro de tabela
                   if ws-ind-jog <= 4 then

      *>               salvar jogador na tabela de jogadores
                       move ws-nome-jogador   to  ws-nome-jog(ws-ind-jog)
                   else
                       move "Quantidade de jogadores completa" to ws-msn
                   end-if
               end-if

           end-perform
           .
       cadastrar-jogadores-exit.
           exit.


      *>------------------------------------------------------------------------
      *>   motor do jogo
      *>------------------------------------------------------------------------
       jogar section.

           perform until ws-sair = "V"
                      or ws-sair = "v"

      *>       executa variando o índice de jogadores ser maior que 4,
      *>       o nome ser espaço ou os jogadores resolverem sair
               perform varying  ws-ind-jog  from 1 by 1 until ws-ind-jog > 4
                                                          or  ws-nome-jog(ws-ind-jog) = spaces
                                                          or  ws-sair = "V"
                                                          or  ws-sair = "v"
      *>           jogador da rodada...
                   move ws-nome-jog(ws-ind-jog)   to   ws-nome-jogador
                   move ws-pontos(ws-ind-jog)     to   ws-pontos-jogador

                   perform sorteia-estado
                   move ws-estado(ws-ind-est)     to   ws-estado-sorteado

                   move space                     to   ws-capital-jog
                   move space                     to   ws-msn

                   display sc-tela-jogar
                   accept sc-tela-jogar


      *>           testa se jogador acertou a resposta
                   if ws-capital-jog = ws-capital(ws-ind-est) then
                         add 1 to ws-pontos(ws-ind-jog)
                         move "Acertou!!!"  to ws-msn
                   else
                         move "Errou!!!"    to ws-msn
                   end-if

                   display sc-tela-jogar
                   accept sc-tela-jogar

               end-perform

           end-perform

           .
       jogar-exit.
           exit.


      *>------------------------------------------------------------------------
      *>   descobrir a proxima posição livre dentro da tabela de jogadores
      *>------------------------------------------------------------------------
       descobrir-prox-ind-jog section.
      *>       executa variando o índice de jogadores ser maior que 4,
      *>       o nome ser espaço
           perform varying ws-ind-jog from 1 by 1 until ws-ind-jog > 4
                                                     or ws-nome-jog(ws-ind-jog) = space
               continue
           end-perform
           .
       descobrir-prox-ind-jog-exit.
           exit.

      *>------------------------------------------------------------------------
      *>   sorteia o estado
      *>------------------------------------------------------------------------
       sorteia-estado section.

            move zero   to   ws-ind-est
            perform until ws-ind-est <> 0
               accept ws-semente from time

               compute ws-num_random = function random(ws-semente)

               multiply ws-num_random by 27 giving ws-ind-est
            end-perform
           .
       sorteia-estado-exit.
           exit.


      *>------------------------------------------------------------------------
      *>   imprimindo relatório final
      *>------------------------------------------------------------------------
       relatorio-final section.

           perform until ws-sair = "X"
                      or ws-sair = "x"

               perform ordenar-jogadores

               move space to ws-msn
               move space to ws-sair

               display sc-tela-relatorio
               accept sc-tela-relatorio

           end-perform

           .
       relatorio-final-exit.
           exit.


      *>------------------------------------------------------------------------
      *>   ordenação da tabela de jogadores
      *>------------------------------------------------------------------------
       ordenar-jogadores section.
           set ws-trocou  to true

           perform until ws-nao_trocou
               move 1           to     ws-ind-jog

               set ws-nao_trocou   to true

      *>       executa variando o índice de jogadores ser maior que 4,
      *>       o nome ser espaço
               perform until ws-ind-jog = 4
               or ws-nome-jog(ws-ind-jog + 1) = space

      *>           critério de ordenação é "pontos do jogador"
                   if ws-pontos(ws-ind-jog) < ws-pontos(ws-ind-jog + 1) then
      *>               faz troca...
                       move ws-jogadores(ws-ind-jog + 1)  to  ws-jogadores-aux
                       move ws-jogadores(ws-ind-jog)      to  ws-jogadores(ws-ind-jog + 1)
                       move ws-jogadores-aux              to  ws-jogadores(ws-ind-jog)

                       set ws-trocou         to  true

                   end-if
                   add  1   to ws-ind-jog
               end-perform
           end-perform

           .
       ordenar-jogadores-exit.
           exit.

      *>------------------------------------------------------------------------
      *>   finalização anormal - erro
      *>------------------------------------------------------------------------
       finaliza-anormal section.

           display erase
           display ws-msn-erro

           stop run
           .
       finaliza-anormal-exit.
           exit.

      *>------------------------------------------------------------------------
      *>   finalização normal
      *>------------------------------------------------------------------------
       finaliza section.
           stop run
           .
       finaliza-exit.
           exit.

