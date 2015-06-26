import ox.CSO._
import ox.Format._
import java.util.Random

/* TODO: 
 * 1. Procurar cadeira;
 * 2. Iniciar PHIL;
 * 3. Jantar;
 * 4. sairRu;
 */

object NomeEspec {
  //--------------------------------  Constantes
  val MAX_ESTUDANTES = 10
  val MAX_CADEIRAS = 3
  val MAX_RU = 5
  val MAX_CAIXAS = 3
  val MAX_CATRACAS = 2

  //-------------------------------- Funções

  // Retorna o proximo estudante de acordo com a contagem modular.
  def proxEstudante(ultimo: Int): Int = {
    (ultimo+1) % MAX_ESTUDANTES
  }
  
  // Retorna o talher à direita do Filósofo.
  def talherDireito(i: Int): Int = (i+1) % MAX_ESTUDANTES

  // Retorna o talher à esquerda do Filósofo.
  def talherEsquerdo(i: Int): Int = if (i == 0) MAX_CADEIRAS-1 else i-1
  
  //-------------------------------- Processos

  /* Adiciona estudantes à fila e os avança, sincronizando com o processo 'Caixas'.
  *  - primeiro: id do primeiro estudante a chegar. 
  *  - comprarTiquete, sairCaixa: canais de sic. com o caixa. 
  *  - chegouFilaCatraca: canal de sic. com a fila para as catracas. */
  def FilaTiquete(primeiro: Int, comprarTiquete: Seq[![Int]], sairCaixa: Seq[?[Int]], chegouFilaCatraca: Seq[![Int]]) = proc {
    var prox: Int = primeiro
    var filaTiq = List[Int]() // Lista representando os estudantes na fila
    var estudante: Int = 0 // Variavel para o estudante no começo da fila
    var caixa: Int = 0 // Variável para o id do caixa no qual um estudante vai comprar o tíquete
    var tiq: Int = 0 // Variavel com o id da catraca para a qual o estudante deve ir

    // 'Escolha interna'
    val seed = new Random(System.currentTimeMillis())
    var res: Int = 0
    
    while (true) {
      println("#" + prox + " chegou no RU.")
      
      res = seed.nextInt(2)+1
      res match {
        case 1 =>
          filaTiq = filaTiq ::: List(prox)
          println("#" + prox + " entrou na fila para comprar o Tiquete")
        case 2 =>
          println("#" + prox + " desistiu da fila")
      }
      prox = proxEstudante(prox)

      if (filaTiq.length > 0) {
        estudante = filaTiq.head

        // Com entrada do console
        /*print("#" + estudante + " digite o caixa em que quer comprar o tíquete (0-" + (MAX_CAIXAS-1) + "): ")
        caixa = Console.readInt()*/
        
        // Aleatório
        caixa = seed.nextInt(MAX_CAIXAS)
        
        comprarTiquete(caixa)!estudante
        tiq = sairCaixa(caixa)?;
        println("#" + estudante + " comprou o tiquete para a catraca #" + tiq)

        chegouFilaCatraca(tiq)!estudante
        
        filaTiq = filaTiq.tail // Atualiza a fila
      }

    }
  }

  /* Avança os estudantes na fila sincronizando com o processo 'FilaTiquete'.
  *  - i: id do caixa.
  *  - comprarTiquete, sairCaixa: canais de sic. com o caixa. */
  def Caixa(i: Int, comprarTiquete: Seq[?[Int]], sairCaixa: Seq[![Int]]) = proc {
    var estudante: Int = 0
    
    while (true) {
      estudante = comprarTiquete(i)? ;
      println("#" + estudante + " entrou no caixa #" + i)

      if (estudante%2 == 1) 
        sairCaixa(i)!0
      else 
        sairCaixa(i)!1
    }
  }

  /* Processos 'Caixa' executando em paralelo. */
  def Caixas(comprarTiquete: Seq[?[Int]], sairCaixa: Seq[![Int]]) = proc {
    (|| (for (i <- 0 until MAX_CAIXAS) yield Caixa(i, comprarTiquete, sairCaixa)))()
  }

  /* Representa a fila correspondente a uma catraca.
  *  - chegouFilaCatraca: canal que comunica o id do estudante que chegou na fila.
  *  - consulta: canal para consultar à Organizadora se a catraca deve ser liberada ou não.
  *  - libera: evento que indica que a catraca deve ser liberada.
  *  - barra: evento que indica que deve ser barrada (caso do RU lotado). */
  def FilaCatraca(i: Int, chegouFilaCatraca: Seq[?[Int]], consulta: ![Unit], libera: ?[Unit], barra: ?[Unit], chegouFilaComida: ![Int]) = proc {
    var filaCat = List[Int]()
    var estudante: Int = 0
    
    while (true) {
      alt ( (true &&& chegouFilaCatraca(i)) =?=> { est => { filaCat = filaCat ::: List(est) 
                                                   println("#" + est + " entrou na fila da catraca #" + i) } } )
      
      if (filaCat.length > 0) {
        estudante = filaCat.head

        consulta!()
        alt ( (true &&& libera) =?=> { x => { filaCat = filaCat.tail     // Atualiza a fila
                                              chegouFilaComida!estudante // Envia o estudante para a Fila da Comida
                                            }
                                     }
            | (true &&& barra) =?=> { x => { println("#" + estudante + " está na fila esperando alguém sair") } }
        )
      }
    }
  }
  
  /* Processos 'FilaCatraca' executando em paralelo. */
  def FilasCatraca(chegouFilaCatraca: Seq[?[Int]], consulta: ![Unit], libera: ?[Unit], barra: ?[Unit], chegouFilaComida: ![Int]) = proc {
    (|| (for (i <- 0 until MAX_CATRACAS) yield FilaCatraca(i, chegouFilaCatraca, consulta, libera, barra, chegouFilaComida)))()
  }

  /* Processo que limita a entrada de estudantes no RU. 
  *  - consulta, libera, barra: sinc. com 'FilaCatraca'.
  *  - saiuRU: evento que indica que algum estudante saiu do RU, usado para atualizar o estado do processo. */
  def Oganizadora(consulta: ?[Unit], libera: ![Unit], barra: ![Unit], saiuRU: ?[Unit]) = proc {
    var total: Int = 0
    
    while (true) {
      alt ( (true &&& consulta) =?=> { x => if (total < MAX_RU) {
                                              total = total + 1
                                              libera!()
                                            } else {
                                              barra!()
                                            }
                                    }
          | (true &&& saiuRU) =?=> {x => total = total - 1}
      )
    }
  }
  
  /* Representa a fila para pegar a comida.
  *  - chegouFilaComida: canal que comunica o id do estudante que chegou na fila. */
  def FilaComida(chegouFilaComida: ?[Int], procurarCadeira: Seq[![Unit]]) = proc {
    var filaCom = List[Int]()
    var estudante: Int = 0

    while (true) {
      alt ( (true &&& chegouFilaComida) =?=> { est => { filaCom = filaCom ::: List(est) 
                                                        println("#" + est + " entrou na fila para pegar a comida") } } )
        
      if (filaCom.length > 0) {
        estudante = filaCom.head
        println("#" + estudante + " pegou o prato")
        println("#" + estudante + " pegou a comida")
        println("#" + estudante + " pegou o suco")

        procurarCadeira(estudante)!()
        
        filaCom = filaCom.tail // Atualiza a fila
      }  
    }
  }
  
  /* Representa o Estudante e é o processo responsável por procurar uma cadeira e realizar o jantar.
   * i: id do estudante.
   * procurarCadeira: evento que indica que o estudante está pronto para procurar por uma cadeira.
   * sentar, levantar, responder: eventos sincronizados com 'Cadeira', verificam se uma cadeira está ocupada e tenta obter acesso.
   * pickup, putdown: eventos de sincronização com os garfos, FORKS.
   * procurarLixeira: evento de sincronização para obter acesso à lixeira.
   * saiuRU: evento que indica que o estudante saiu do restaurante. */
  def Estudante(i: Int, procurarCadeira: Seq[?[Unit]], sentar: Seq[![Unit]], levantar: Seq[![Unit]], responder: Seq[?[Boolean]], pickup: Seq[?[Unit]], putdown: Seq[![Unit]], procurarLixeira: ![Int], saiuRU: ![Unit]) = proc {
    var pronto: Boolean = false // Pronto para procurar uma cadeira
    var sentado: Boolean = false
    
    var cadeira: Int = 0 // Id da cadeira em que está sentado
    
    // Flags para os garfos
    var left: Boolean = false
    var right: Boolean = false
    
    while (true) {
      if (pronto && !sentado) {
        var c = 0
        
        // Procura alguma cadeira livre
        while (c < MAX_CADEIRAS && !sentado) {
          println("#" + i + " tentando sentar na Cadeira #" + c)
          sentar(c)!()
          val r = responder(c)?;
          
          if (r) {
            println("#" + i + " conseguiu sentar na Cadeira #" + c)
            sentado = true
            cadeira = c
          } else {
            println("#" + i + " não conseguiu sentar na Cadeira #" + c)
            c = c + 1
          }
        }
      } else if (sentado){
        var act: Int = 0
        
        if (!left) {
          print("#" + i + " pegar o garfo da esquerda (" + talherEsquerdo(cadeira) + ")? Digite 0 ou 1: ")
          act = Console.readInt()
          
          if (act == 1) {
            pickup(talherEsquerdo(cadeira))?;
            println("#" + i + " pegou o garfo da esquerda")
            left = true
          }
        } else {
          print("#" + i + " devolver o garfo da esquerda (" + talherEsquerdo(cadeira) + ")? Digite 0 ou 1: ")
          act = Console.readInt()
          
          if (act == 1) {
            putdown(talherEsquerdo(cadeira))!();
            println("#" + i + " devolveu o garfo da esquerda")
            left = false
          }
        }

        if (!right) {
          print("#" + i + " pegar o garfo da direita (" + talherDireito(cadeira) + ")? Digite 0 ou 1: ")
          act = Console.readInt()
          
          if (act == 1) {
            pickup(talherDireito(cadeira))?;
            println("#" + i + " pegou o garfo da direita")
            right = true
          }
        } else {
          print("#" + i + " devolver o garfo da direita (" + talherDireito(cadeira) + ")? Digite 0 ou 1: ")
          act = Console.readInt()
          
          if (act == 1) {
            putdown(talherDireito(cadeira))!();
            println("#" + i + " devolveu o garfo da direita")
            left = false
          }
        }
        
        if (left && right) { // Possui acesso aos dois garfos
          println("#" + i + " está comendo")
        } else if (!left && !right){ // Já devolveu os dois garfos
          print("#" + i + " vai sair do RU? Digite 0 ou 1:")
          act = Console.readInt()
          
          if (act == 1) {
            procurarLixeira!i
            saiuRU!()
            println("#" + i + " saiu do RU")
            exit // Termina o processo
          }
        }
        
      } else {
        val r = procurarCadeira(i)?; // Espera a indicação para começar a procurar cadeira
        pronto = true
        println("#" + i + " está procurando uma cadeira")
      }
    }
  }
  
  /* Processos 'Estudante' executando em paralelo. */
  def Estudantes(procurarCadeira: Seq[?[Unit]], sentar: Seq[![Unit]], levantar: Seq[![Unit]], responder: Seq[?[Boolean]], pickup: Seq[?[Unit]], putdown: Seq[![Unit]], procurarLixeira: ![Int], saiuRU: ![Unit]) = proc {
    (|| ( for (i <- 0 until MAX_ESTUDANTES) yield Estudante(i, procurarCadeira, sentar, levantar, responder, pickup, putdown, procurarLixeira, saiuRU) ))() 
  }
  
  /* Processo que representa um garfo.
   * pickup: tentar pegar o garfo.
   * putdown: devolver o garfo. */
  def FORK(i: Int, pickup: Seq[![Unit]], putdown: Seq[?[Unit]]) = proc {
    while(true) {
      alt( (true &&& pickup(i)) =!=> { () } ==> { putdown(i)? }
         | (true &&& pickup(talherEsquerdo(i))) =!=> { () } ==> { putdown(talherEsquerdo(i))? } )
    }
  }
  
  /* Processos 'FORK' executando em paralelo. */
  def FORKS(pickup: Seq[![Unit]], putdown: Seq[?[Unit]]) = proc {
    (|| (for (i <- 0 until MAX_CADEIRAS) yield FORK(i, pickup, putdown)))()
  }
  
  /* Representa uma cadeira do RU. Deve ser obtida por algum filósofo para que ele possa iniciar o jantar. 
   * i: id da cadeira.
   * sentar: evento que indica que alguém quer obter a cadeira.
   * levantar: evento que indica que o filósofo saiu da cadeira.
   * responder: evento utilizado pela cadeira para confirmar ou não o sucesso em obter a cadeira. */
  def Cadeira(i: Int, sentar: Seq[?[Unit]], levantar: Seq[?[Unit]], responder: Seq[![Boolean]]) = proc {
    var ocupada = false
    
    while (true) {
      alt ( (ocupada &&& sentar(i)) =?=> {x => { responder(i)!false } }
          | (!ocupada &&& sentar(i)) =?=> {x => { ocupada = true
                                                  responder(i)!true } }
          | (ocupada &&& levantar(i)) =?=> {x => { ocupada = false } } )
    }
  }
  
  /* Processos 'Cadeira' executando em paralelo. */
  def Cadeiras(sentar: Seq[?[Unit]], levantar: Seq[?[Unit]], responder: Seq[![Boolean]]) = proc {
    (|| (for (i <- 0 until MAX_CADEIRAS) yield Cadeira(i, sentar, levantar, responder) ) )();
  }
  
  /* Processo que representa a 'Lixeira' */
  def Lixeira(procurarLixeira: ?[Int]) = proc {
    var estudante: Int = 0

    while (true) {
      estudante = procurarLixeira?;
      println("#" + estudante + " jogou fora o resto de comida e o copo.")
      println("#" + estudante + " jogou devolveu a bandeja e o prato.")
    }
  }
  
  def RU() = proc {

    // Declarar canais...    
    val comprarTiquete = ManyMany[Int](MAX_CAIXAS)
    val sairCaixa = ManyMany[Int](MAX_CAIXAS)
    val chegouFilaCatraca = ManyMany[Int](MAX_CAIXAS)
    
    val consulta = ManyMany[Unit]
    val libera = ManyMany[Unit]
    val barra = ManyMany[Unit]
    val saiuRU = ManyMany[Unit]
    
    val chegouFilaComida = ManyMany[Int]
    
    val procurarCadeira = ManyMany[Unit](MAX_ESTUDANTES)
    val sentar = ManyMany[Unit](MAX_CADEIRAS)
    val levantar = ManyMany[Unit](MAX_CADEIRAS)
    
    val responder = ManyMany[Boolean](MAX_CADEIRAS)
    
    val pickup = ManyMany[Unit](MAX_CADEIRAS)
    val putdown = ManyMany[Unit](MAX_CADEIRAS)
    
    val procurarLixeira = ManyMany[Int]
    
    (FilaTiquete(0, comprarTiquete, sairCaixa, chegouFilaCatraca)() 
        ||
      Caixas(comprarTiquete, sairCaixa)()
        ||
      FilasCatraca(chegouFilaCatraca, consulta, libera, barra, chegouFilaComida)()
        ||
      Oganizadora(consulta, libera, barra, saiuRU)()
        ||
      FilaComida(chegouFilaComida, procurarCadeira)()
        ||
      Estudantes(procurarCadeira, sentar, levantar, responder, pickup, putdown, procurarLixeira, saiuRU)()
        ||
      Cadeiras(sentar, levantar, responder)()
        || 
      FORKS(pickup, putdown)()
        ||
      Lixeira(procurarLixeira)()
    )()
    
  }
  
  def main(args: Array[String]) {
    (RU())()
    exit
  }
} 
