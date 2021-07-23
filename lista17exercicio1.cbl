      $set sourceformat"free"
      *> divis�o de identifica��o do programa
       identification division.
       program-id. "lista17exercicio1".
       author. "Camila da Rosa Hostin".
       installation. "PC".
       date-written. 28/07/2020.
       date-compiled. 28/07/2020.

      *> divis�o para configura��o do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----declara��o dos recursos externos
       input-output section.
       file-control.

           select arqCadastroAlunos assign to "arqCadastroAlunos.dat"
           organization is indexed
           access mode is dynamic
           lock mode is automatic
           record key is fd-cod-aluno
           file status is ws-fs-arqCadastroAlunos.

       i-o-control.

      *> declara��o de vari�veis
       data division.

      *>----variaveis de arquivos
       file section.

       fd arqCadastroAlunos.

       01 fd-alunos.
           05 fd-cod-aluno                         pic  9(03).
           05 fd-aluno                             pic  x(25).
           05 fd-endereco                          pic  x(35).
           05 fd-mae                               pic  x(25).
           05 fd-pai                               pic  x(25).
           05 fd-telefone                          pic  x(15).
           05 fd-nota-g.
               10 fd-notas occurs 4.
                   15 fd-nota                      pic  9(02)v99.

      *>----variaveis de trabalho
       working-storage section.

       77 ws-fs-arqCadastroAlunos                  pic  x(02).

       01 ws-bynary-staus redefines
            ws-fs-arqCadastroAlunos                pic  9(04).

      *>  vari�veis do cadastro do aluno
       01 ws-alunos.
           05 ws-cod-aluno                         pic  9(03).
           05 ws-nome-aluno                        pic  x(25).
           05 ws-endereco-aluno                    pic  x(35).
           05 ws-nome-mae                          pic  x(25).
           05 ws-nome-pai                          pic  x(25).
           05 ws-tel-pais                          pic  x(15).
      *>  vari�veis nota
           05 ws-nota-g.
               10 ws-notas occurs 4.
                   15 ws-nota                      pic  9(02)v99.

       01 ws-uso-geral.
           05 ws-menu                              pic  x(01) value 'S'.
           05 ws-opcao                             pic  x(01).
           05 ws-ind-nota                          pic  9(01).

      *>  vari�veis de mensagem de erro
       01 ws-msn-erro.
           05 ws-msn-erro-offset                   pic  9(04).
           05 filler                               pic  x(01) value "-".
           05 ws-msn-erro-cod                      pic  x(02).
           05 filler                               pic  x(01) value space.
           05 ws-msn-erro-text                     pic  x(42).

      *>----variaveis para comunica��o entre programas
       linkage section.

      *>----declara��o de tela
       screen section.

      *>declara��o do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  procedimentos de inicializa��o
      *>------------------------------------------------------------------------
       inicializa section.

       *>  open i-o abre o arquivo para leitura e escrita
           open i-o arqCadastroAlunos
      *>       tratamento de erro
               if ws-fs-arqCadastroAlunos  <> '00'
               and ws-fs-arqCadastroAlunos <> '05' then
      *>           mensagem de erro
                   move 1 to ws-msn-erro-offset
                   move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
                   move 'Erro ao abrir arq.arqCadastroAlunos' to ws-msn-erro-text
      *>           finalizar programa por erro
                   perform finaliza-anormal
               end-if
           .
       inicializa-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  processamento principal
      *>------------------------------------------------------------------------
       processamento section.

      *>   rodar programa at� que a condi��o de sa�da seja n�o
           perform until ws-menu <> 'S'

      *>       limpar tela
               display erase

      *>       menu de consulta
               display 'Digite:'
               display 'A - Cadastro de Alunos'
               display 'B - Cadastro de Notas'
               display 'C - Consulta Cadastro'
               display 'E - Deletar Cadastro'
               display 'F - Alterar Cadastro'
               accept ws-opcao
               move function upper-case (ws-opcao) to ws-opcao

      *>       evaluate p/ mandar o programa p/ as sections
               evaluate ws-opcao
                   when = 'A'
                       perform cadastro-aluno
                   when = 'B'
                       perform cadastro-notas
                   when = 'C'
                       perform consulta-cadastro
                   when = 'D'
                       perform deletar-aluno
                   when = 'E'
                       perform alterar-aluno
                   when other
                       display 'Opcao Invalida'
               end-evaluate

      *>       condi��o de sa�da
               display 'Quer continuar? S-im/N-ao'
               accept ws-menu
               move function upper-case(ws-menu) to ws-menu

           end-perform

           .
       processamento-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  cadastro de aluno
      *>------------------------------------------------------------------------
       cadastro-aluno section.

      *>   rodar programa at� que a condi��o de sa�da seja n�o
           perform until ws-menu <> 'S'

      *>       limpar tela
               display erase

               display '---------- Cadastro de Alunos ----------'

      *>       cadastro do nome do aluno
               display 'Informe o Codigo do Aluno: '
               accept ws-cod-aluno
               display 'Informe o Nome do Aluno: '
               accept ws-nome-aluno

      *>       cadastro endere�o
               display 'Informe o Endereco: '
               accept ws-endereco-aluno

      *>       cadastro informa��es dos pais
               display 'Informe o Nome do Pai: '
               accept ws-nome-pai
               display 'Informe o Nome da Mae: '
               accept ws-nome-mae
               display 'Telefone dos Pais: '
               accept ws-tel-pais

      *> -------------  salvar dados no arquivo

      *>       escreve os dados no arquivo
               write fd-alunos from ws-alunos

      *>       tratamento de erro
               if ws-fs-arqCadastroAlunos <> '00' then
      *>           mensagem de erro
                   move 2 to ws-msn-erro-offset
                   move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
                   move 'Erro ao Gravar arq.arqCadastroAlunos' to ws-msn-erro-text
      *>           fechar arquivo quando d� erro
                   perform finaliza-anormal
               end-if

      *> -------------

      *>       condi��o de sa�da
               display 'Continuar Cadastrando? S-im/N-ao'
               accept ws-menu
               move function upper-case(ws-menu) to ws-menu

           end-perform

           .
       cadastro-aluno-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  cadastro de notas
      *>------------------------------------------------------------------------
       cadastro-notas section.

      *>   rodar programa at� que a condi��o de sa�da seja n�o
           perform until ws-menu <> 'S'

      *>       limpar tela
               display erase

               display '---------- Cadastro de Notas ----------'
               display 'Informe o Codigo do Aluno: '
               accept ws-cod-aluno

               if ws-cod-aluno = space then
                   display 'Aluno nao Cadastrado'
               end-if

      *>   cadastro das notas
               display 'Informe a nota 1: '
               accept ws-nota(1)
               display 'Informe a nota 2: '
               accept ws-nota(2)
               display 'Informe a nota 3: '
               accept ws-nota(3)
               display 'Informe a nota 4: '
               accept ws-nota(4)

      *> -------------  salvar dados no arquivo

      *>       preenche o fd-cod-aluno
               move ws-cod-aluno to fd-cod-aluno

      *>       ler arquivo
               read arqCadastroAlunos

      *>       tratamento de erro
               if ws-fs-arqCadastroAlunos <> '00' then
                   if ws-fs-arqCadastroAlunos = '23' then
      *>               mensagem de erro
                       display 'C�digo Inv�lido'
                   else
      *>               mensagem de erro
                       move 3 to ws-msn-erro-offset
                       move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
                       move 'Erro ao Ler arq.arqCadastroAlunos' to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               else
      *>           move dados da vari�vel ws para fd
                   move ws-nota-g to fd-nota-g
      *>           sobreescrever o arquivo
                   rewrite fd-alunos
      *>           tratamento de erro
                   if ws-fs-arqCadastroAlunos <> '00' then
      *>               mensagem de erro
                       move 4 to ws-msn-erro-offset
                       move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
                       move 'Erro ao Gravar arq.arqCadastroAlunos' to ws-msn-erro-text
      *>               fechar arquivo quando d� erro
                       perform finaliza-anormal
                   end-if
               end-if

      *> -------------

      *>       condi��o de sa�da
               display 'Continuar Cadastrando? S-im/N-ao'
               accept ws-menu
               move function upper-case(ws-menu) to ws-menu

           end-perform
           .
       cadastro-notas-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  consultar cadastro
      *>------------------------------------------------------------------------
       consulta-cadastro section.

           display '---------- Opcoes de Cadastro ----------'
           display '1-Consulta Indexada'
           display '2- Consulta Sequencial'
           accept ws-opcao

           evaluate ws-opcao
               when = '1'
                   perform consulta-cadastro-indexada
               when = '2'
                   perform consulta-cadastro-seq-next
               when other
                   display 'Opcao Invalida'
           end-evaluate
           .
       consulta-cadastro-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  consultar cadastro - indexada
      *>------------------------------------------------------------------------
       consulta-cadastro-indexada section.

      *>   rodar programa at� que a condi��o de sa�da seja n�o
           perform until ws-menu <> 'S'

      *>       limpar tela
               display erase

               display '---------- Consultar Cadastro ----------'
               display 'Informe o Codigo do Aluno: '
               accept ws-cod-aluno

      *> -------------  ler dados no arquivo - indexada

      *>       movendo conte�dos das vari�veis ws para fd
               move ws-cod-aluno to fd-cod-aluno

      *>       ler arquivo
               read arqCadastroAlunos

      *>       tratamento de erro
               if ws-fs-arqCadastroAlunos <> '00' then
                   if ws-fs-arqCadastroAlunos = '23' then
      *>               mensagem de erro
                       display 'Codigo Invalido!'
                   else
      *>               mensagem de erro
                       move 5 to ws-msn-erro-offset
                       move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
                       move 'Erro ao Ler arq. arqCadastroAlunos' to ws-msn-erro-text
      *>               fechar arquivo quando d� erro
                       perform finaliza-anormal
                   end-if
               else
      *>           movendo conte�dos das vari�veis fd para ws
                   move fd-alunos to ws-alunos
      *>           apresenta��o dos dados do aluno
                   display 'Codigo do Aluno: ' ws-cod-aluno
                   display 'Nome do Aluno: ' ws-nome-aluno
                   display 'Endere�o: ' ws-endereco-aluno
                   display 'Nome do Pai: ' ws-nome-pai
                   display 'Nome da Mae: ' ws-nome-mae
                   display 'Telefone dos Pais: ' ws-tel-pais
                   display 'Nota 1: ' ws-nota(1)
                   display 'Nota 2: ' ws-nota(2)
                   display 'Nota 3: ' ws-nota(3)
                   display 'Nota 4: ' ws-nota(4)

               end-if

      *> -------------

      *>       condi��o de sa�da
               display 'Deseja Continuar Consultando? S-im/N-ao'
               accept ws-menu
               move function upper-case(ws-menu) to ws-menu

           end-perform

          .
       consulta-cadastro-indexada-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  consultar cadastro - de forma sequencial - next
      *>------------------------------------------------------------------------
       consulta-cadastro-seq-next section.

      *>   para saber o ponto de in�cio
           perform consulta-cadastro-indexada

      *>   rodar programa at� que a condi��o de sa�da seja n�o
           perform until ws-menu <> 'S'

      *> -------------  ler dados no arquivo de forma sequencial - next

      *>       ler arquivo de forma sequencial - next
               read arqCadastroAlunos next

      *>       tratamento de erro
               if ws-fs-arqCadastroAlunos <> '00' then
                   if ws-fs-arqCadastroAlunos = '10' then
                   perform consulta-cadastro-seq-prev
                   else
      *>               mensagem de erro
                       move 6 to ws-msn-erro-offset
                       move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
                       move 'Erro ao Ler arq. arqCadastroAlunos' to ws-msn-erro-text
      *>               fechar arquivo quando d� erro
                       perform finaliza-anormal
                   end-if
               else
      *>           movendo conte�dos das vari�veis fd para ws
                   move fd-alunos to ws-alunos
      *>           apresenta��o dos dados do aluno
                   display 'Codigo do Aluno: ' ws-cod-aluno
                   display 'Nome do Aluno: ' ws-nome-aluno
                   display 'Endere�o: ' ws-endereco-aluno
                   display 'Nome do Pai: ' ws-nome-pai
                   display 'Nome da Mae: ' ws-nome-mae
                   display 'Telefone dos Pais: ' ws-tel-pais
                   display 'Nota 1: ' ws-nota(1)
                   display 'Nota 2: ' ws-nota(2)
                   display 'Nota 3: ' ws-nota(3)
                   display 'Nota 4: ' ws-nota(4)

               end-if

      *> -------------

      *>       condi��o de sa�da
               display 'Deseja Continuar Consultando? S-im/N-ao'
               accept ws-menu
               move function upper-case(ws-menu) to ws-menu

           end-perform


           .
       consulta-cadastro-seq-next-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  consultar cadastro - previous
      *>------------------------------------------------------------------------
       consulta-cadastro-seq-prev section.

      *>   para saber o ponto de in�cio
           perform consulta-cadastro-indexada

      *>   rodar programa at� que a condi��o de sa�da seja n�o
           perform until ws-menu <> 'S'

      *> -------------  ler dados no arquivo de forma sequencial - previous

      *>       ler arquivo de forma sequencial
               read arqCadastroAlunos previous

      *>       tratamento de erro
               if ws-fs-arqCadastroAlunos <> '00' then
                   if ws-fs-arqCadastroAlunos = '10' then
                       perform consulta-cadastro-seq-next
                   else
      *>               mensagem de erro
                       move 7 to ws-msn-erro-offset
                       move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
                       move 'Erro ao Ler arq. arqCadastroAlunos' to ws-msn-erro-text
      *>               fechar arquivo quando d� erro
                       perform finaliza-anormal
                   end-if
               else
      *>           movendo conte�dos das vari�veis fd para ws
                   move fd-alunos to ws-alunos
      *>           apresenta��o dos dados do aluno
                   display 'Codigo do Aluno: ' ws-cod-aluno
                   display 'Nome do Aluno: ' ws-nome-aluno
                   display 'Endere�o: ' ws-endereco-aluno
                   display 'Nome do Pai: ' ws-nome-pai
                   display 'Nome da Mae: ' ws-nome-mae
                   display 'Telefone dos Pais: ' ws-tel-pais
                   display 'Nota 1: ' ws-nota(1)
                   display 'Nota 2: ' ws-nota(2)
                   display 'Nota 3: ' ws-nota(3)
                   display 'Nota 4: ' ws-nota(4)
               end-if

      *> -------------

      *>       condi��o de sa�da
               display 'Deseja Continuar Consultando? S-im/N-ao'
               accept ws-menu
               move function upper-case(ws-menu) to ws-menu

           end-perform
           .
       consulta-cadastro-seq-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  alterar cadastro
      *>------------------------------------------------------------------------
       alterar-aluno section.


      *>   rodar programa at� que a condi��o de sa�da seja n�o
           perform until ws-menu <> 'S'

      *>       limpar tela
               display erase

      *>       informar o c�digo do aluno
               display 'Informe o Codigo do Aluno a Ser Alterado: '
               accept ws-cod-aluno

      *>       movendo informa��o da vari�vel ws para fd
               move ws-cod-aluno to fd-cod-aluno

               read arqCadastroAlunos

      *>       tratamento de erro
               if ws-fs-arqCadastroAlunos <> '00' then
                   if ws-fs-arqCadastroAlunos = '23' then
                       display 'Codigo do Aluno Inexistente'
                   else
      *>               mensagem de erro
                       move 8 to ws-msn-erro-offset
                       move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
                       move 'Erro ao ler arq. arqCadastroAlunos' to ws-msn-erro-text
      *>               fechar arquivo quando d� erro
                       perform finaliza-anormal
                   end-if
               else

      *>           movendo informa��o da vari�vel fd para ws
                   move fd-alunos to ws-alunos

      *>           menu altera��o cadastro
                   display '---Alterar Cadastro---'
                   display '1 - aluno'
                   display '2 - endereco'
                   display '3 - nome pai'
                   display '4 - nome mae'
                   display '5 - telefone'
                   display '6 - notas'
                   accept ws-opcao

                   evaluate ws-opcao
                       when = '1'
                           display 'Nome do Aluno: '
                           accept ws-nome-aluno
                       when = '2'
                           display 'Endere�o: '
                           accept ws-endereco-aluno
                       when = '3'
                           display 'Nome do Pai: '
                           accept ws-nome-pai
                       when = '4'
                           display 'Nome da Mae: '
                           accept ws-nome-mae
                       when = '5'
                           display 'Telefone dos Pais: '
                           accept ws-tel-pais
                       when = '6'
                           display 'Qual Nota? (1 - 2 - 3 -4) '
                           accept ws-ind-nota
                           display 'Nota: '
                           accept ws-nota(ws-ind-nota)
                       when other
                           display 'Opcao Inv�lida'
                   end-evaluate

                   move ws-alunos to fd-alunos

      *>           sobreescrever o arquivo
                   rewrite fd-alunos
      *>           tratamento de erro
                   if ws-fs-arqCadastroAlunos <> '00' then
      *>               mensagem de erro
                       move 9 to ws-msn-erro-offset
                       move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
                       move 'Erro ao gravar arq.arqCadastroAlunos' to ws-msn-erro-text
      *>               fechar arquivo quando d� erro
                       perform finaliza-anormal
                   end-if

               end-if

      *> -------------

      *>       condi��o de sa�da
               display 'Deseja Alterar Mais Algum Cadastro? S-im/N-ao'
               accept ws-menu
               move function upper-case(ws-menu) to ws-menu

           end-perform

           .
       alterar-aluno-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  deletar cadastro
      *>------------------------------------------------------------------------
       deletar-aluno section.

           display erase

           perform consulta-cadastro-indexada

      *>   rodar programa at� que a condi��o de sa�da seja n�o
           perform until ws-menu <> 'S'

               display 'Informe o Codigo do Aluno a Ser Exclu�do: '
               accept ws-cod-aluno

      *> -------------  deletar dados no arquivo

      *>       movendo informa��o da vari�vel ws para fd
               move ws-cod-aluno to fd-cod-aluno

      *>       deletar arquivo
               delete arqCadastroAlunos

      *>       tratamento de erro
               if ws-fs-arqCadastroAlunos <> '00' then
                   if ws-fs-arqCadastroAlunos = '23' then
                       display 'Aluno Informado Invalido'
                   else
      *>               mensagem de erro
                       move 10 to ws-msn-erro-offset
                       move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
                       move 'Erro ao apagar arq.arqCadastroAlunos' to ws-msn-erro-text
      *>               fechar arquivo quando d� erro
                       perform finaliza-anormal
                   end-if
               end-if

      *> -------------

      *>       condi��o de sa�da
               display 'Deseja Deletar Mais Algum Cadastro? S-im/N-ao'
               accept ws-menu
               move function upper-case(ws-menu) to ws-menu

           end-perform

           .
       deletar-aluno-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  finaliza��o anormal - erro
      *>------------------------------------------------------------------------
       finaliza-anormal section.

           display erase
           display ws-msn-erro.

           stop run
           .

       finaliza-anormal-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  finaliza��o
      *>------------------------------------------------------------------------
       finaliza section.

      *>   fechar arquivo
           close arqCadastroAlunos

      *>   tratamento de erro
           if ws-fs-arqCadastroAlunos <> '00' then
      *>       mensagem de erro
               move 11 to ws-msn-erro-offset
               move ws-fs-arqCadastroAlunos to ws-msn-erro-cod
               move 'Erro ao fechar arq.arqCadastroAlunos' to ws-msn-erro-text
      *>       fechar arquivo quando d� erro
               perform finaliza-anormal
           end-if

           stop run
           .

       finaliza-exit.
           exit.

