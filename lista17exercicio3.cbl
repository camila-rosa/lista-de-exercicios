      $set sourceformat"free"

      *>divisão de identificação do programa
       identification division.
      *>---program-id é uma informação obrigatória---
       program-id. "lista17exercicio3".
       author. "Camila da Rosa Hostin".
       installation. "PC".
       date-written. 28/07/2020.
       date-compiled. 28/07/2020.

      *>divisão para configuração de ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>declaração de recursos externos
       input-output section.
       file-control.

           select arqTemperatura assign to 'arqTemperatura.txt'
           organization is line sequential
           access mode is sequential
           lock mode is automatic
           file status is ws-fs-arqTemperatura.

       i-o-control.

      *>declaração de variáveis
       data division.
      *>-data division, tem 4 sessões possíveis-
      *>---variáveis de arquivos---
       file section.

       fd arqTemperatura.
       01 fd-relatorioTemp.
          05 fd-temperatura                        pic s9(02)v99.

      *>---variáveis de trabalho---
       working-storage section.

       77 ws-fs-arqTemperatura                     pic  9(02).

       01 ws-temperaturas occurs 30.
          05 ws-temp                               pic s9(02)v99.

       77 ws-media-temp                            pic s9(02)v99.

       77 ws-temp-total                            pic s9(03)v99.

       77 ws-dia                                   pic  9(02).
       77 ws-ind-temp                              pic  9(02).
       77 ws-sair                                  pic  x(01).

      *>  mensagens de erro
       01 ws-msn-erro.
           05 ws-msn-erro-offset                   pic  x(04).
           05 filler                               pic  x(01) value '-'.
           05 ws-msn-erro-cod                      pic  x(1).
           05 filler                               pic  x(02) value space.
           05 ws-msn-erro-text                     pic  x(42).

      *>---variáveis para comunicação entre programas---
       linkage section.
      *>---declaração de tela---
       screen section.

      *>declaração do corpo do programa
       procedure division.

      *>3. Crie um programa para armazenar informações meteorológicas.
      *> - Crie um vetor para armazenar 30 temperaturas.
      *> - Calcular a média das temperaturas.
      *> - O usuário pode informar um dia qualquer e o programa deve
      *>dizer se a temperatura desse dia estava acima ou abaixo da média.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>--------------------------------------------------------------------<*
      *> procedimentos de inicialização
      *>--------------------------------------------------------------------<*
       inicializa section.

      *>   open input abre o arquivo para leitura
           open input arqTemperatura.
      *>   tratamento de erro - file status diferente de 0, erro ao abrir arquivo
           if ws-fs-arqTemperatura <> 0 then
               move 1 to ws-msn-erro-offset
               move ws-fs-arqTemperatura to ws-msn-erro-cod
               move 'Erro ao Abrir Arquivo arqTemperatura' to ws-msn-erro-text
               perform finaliza-anormal
           end-if

      *>   executa variando o índice de temperatura até o índice ser maior que 30
           perform varying ws-ind-temp from 1 by 1 until ws-fs-arqTemperatura = 10
                                                               or ws-ind-temp > 30

      *>       lê o arquivo de temperatura
               read arqTemperatura into ws-temperaturas(ws-ind-temp)
      *>       tratamento de erro - file status diferente de 0 e 10
               if ws-fs-arqTemperatura <> 0
               and ws-fs-arqTemperatura <> 10  then
                   move 2 to ws-msn-erro-offset
                   move ws-fs-arqTemperatura to ws-msn-erro-cod
                   move 'Erro ao Ler Arquivo arqTemperatura' to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

           end-perform

      *>   fechar arquivo
           close arqTemperatura.
      *>   tratamento de erro  - file status diferente de 0
           if ws-fs-arqTemperatura <> 0 then
               move 3 to ws-msn-erro-offset
               move ws-fs-arqTemperatura to ws-msn-erro-cod
               move 'Erro ao Fechar Arquivo arqTemperatura' to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           .
       inicializa-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  processamento principal
      *>------------------------------------------------------------------------
       processamento section.

      *>   chamando rotina de calculo da média de temperatura
           perform calc-media-temp

      *>   menu do sistema
           perform until ws-sair = "S"
                      or ws-sair = "s"
               display erase

      *>       informar o dia
               display "Dia a ser testado: "
               accept ws-dia

               if  ws-dia > 1
               and ws-dia < 30 then
                   if ws-temp(ws-dia) > ws-media-temp then
                       display "A temperatura do dia " ws-dia " esta acima da media"
                   else
                   if ws-temp(ws-dia) < ws-media-temp then
                           display "A temperatura do dia " ws-dia " esta abaixo da media"
                   else
                           display "A temperatura esta na media"
                   end-if
                   end-if
               else
      *>           se informar um dia menos que 1 e maior que 30
                   display "Dia fora do intervalo valido (1-30)"
               end-if

      *>       condição de saída
               display "'T'estar outra temperatura"
               display "'S'air"
               accept ws-sair
               move function upper-case(ws-sair) to ws-sair

           end-perform
           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  calculo da média de temperatura
      *>------------------------------------------------------------------------
       calc-media-temp section.

      *>   inicializando variável de temperatura total
           move 0 to ws-temp-total

      *>   executa variando o índice de temperatura até o índice ser maior que 30
           perform varying ws-ind-temp from 1 by 1 until ws-ind-temp > 30

      *>       somando todas as temperaturas
               compute ws-temp-total = ws-temp-total + ws-temp(ws-ind-temp)

           end-perform

      *>   calculo da média da temperatura
           compute ws-media-temp = ws-temp-total / 30

           .
       calc-media-temp-exit.
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


