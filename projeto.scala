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
  val MAX_EXTUDANTES = 5
  val MAX_CADEIRAS = 3
  val MAX_RU = 5
  val MAX_CAIXAS = 3

  //-------------------------------- Funções

  // Retorna o proximo estudante de acordo com a contagem modular.
  def proxEstudante(ultimo: Int): Int = {
    (ultimo+1) % MAX_EXTUDANTES
  }
  
  // Retorna o talher à direita do Filósofo.
  def talherDireito(t_esquerdo: Int): Int = {
    (t_esquerdo+1) % MAX_CADEIRAS
  }

  // Retorna o talher à esquerda do Filósofo.
  def talherEsquerdo(t_direito: Int): Int = {
    if (t_direito == 0) MAX_CADEIRAS-1
    else t_direito-1
  }

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

        print("#" + estudante + " digite o caixa em que quer comprar o tíquete: ")
        caixa = Console.readInt()

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
      println("#" + estudante + "entrou no caixa #" + i)

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
  def FilaComida(chegouFilaComida: ?[Int]) = proc {
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

        // TODO: procurar cadeira

        filaCom = filaCom.tail // Atualiza a fila
      }  
    }
  }
  
  def main(args: Array[String]) {
    // Declarar canais...
    // Chamar o processo principal...
    exit
  }
} 
