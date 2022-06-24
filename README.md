Integrantes: Thomas do Vale - Matricula: 0015131 
             Lucas Guimarães Bernardes - Matricula: 0015122

#Para executar o codigo:
 
 É necessária a instalação da ferramenta Stack, parte do toolkit oficial para desenvolvimento
 em haskell.
 Com o Stack instalado é possível realizar a compilação do projeto utilizando o comando
 $ stack build
 ou até mesmo a execução direta utilizando o
 $ stack run
 Com isso o projeto pode ser executado como
 $ stack run <"caminho arquivo de entrada"> <"caminho arquivo saida">

 Voce tambem pode omitir os parametros <"caminho arquivo de entrada"> e <"caminho arquivo saida">.
 Nesse caso, o programa procurará por um arquivo de entrada com nome 'input.csv' e salvará
 as respostas das consultas num arquivo de nome 'output.txt'. Caso nao haja um arquivo de entrada com o nome correto, o programa
 falhará ao tentar ser executado. O arquivo de entrada pode ter qualquer extensão, mas deve
 estar formatado internamente como um csv.

#Escrita dos dados:
 Foi feita utilizando a funcão padrão do haskell 'writeFile' que pede como argumento o nome
 do arquivo de saida e também a string a ser escrita.

 No programa, cada vez que o usuário faz uma consulta, a string contendo a resposta de tal
 consulta é adicionada a um vetor auxiliar 'maux'. Quando o usuário vai gerar o relatório das
 respostas de suas consultas, o conteudo do vetor 'maux' é salvo no arquivo desejado.

#Leitura dos dados:
 Foi feita utilizando uma biblioteca externa ao Haskell chamada 'Cassava'.
 Utilizamos o método 'decodeByName' da biblioteca, que decodifica o csv e transforma
 cada linha do mesmo em um objeto 'Compra' definido por nós no código. O método então retorna
 um Vector do tipo 'Compra', contendo todos os registros do arquivo.
 
 A partir daí, passamos esse Vector para o menu e para as os métodos que executam as operações,
 utilizando funções que retornam o Vector convertido em uma lista quando necessário, 
 para facilitar o manuseio.
 


A opcao2 (Quantidade comprada de um produto) da a resposta na unidade do produto, quando possível.

A opcao7 (Salvar no relatorio) só salva caso o buffer nao estiver vazio. E apos salvar o relatório,
reseta o buffer que armazena as respostas das consultas

Fizemos dois extras, a consulta de quantidade comprada no dia e a consulta de quantidade total gasta
(opcoes 8 e 9)

