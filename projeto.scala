import ox.CSO._
import ox.Format._
import scala.collection.mutable.MutableList
import java.util.Random

object NomeEspec {
	//--------------------------------  Constantes
	val MAX_EXTUDANTES = 5
	val MAX_MESA = 3
	val MAX_RU = 5

	//-------------------------------- Funções

	// Retorna o proximo estudante de acordo com a contagem modular.
	def proxEstudante(ultimo: Int) {
		(ultimo+1) % MAX_EXTUDANTES
	}
	
	// Retorna o talher à direita do Filósofo.
	def talherDireito(t_esquerdo: Int) {
		(t_esquerdo+1) % MAX_MESA
	}

	// Retorna o talher à esquerda do Filósofo.
	def talherEsquerdo(t_direito: Int) {
		if (t_direito == 0) MAX_MESA-1
		else t_direito-1
	}

	//-------------------------------- Processos

	/* Adiciona estudantes à fila.
	*  - primeiro: id do primeiro estudante a chegar.
	*  - filaTiq: lista compartilhada representando os estudantes que estão na fila. */
	def novosEstudantes(primeiro: Int, filaTiq: MutableList[Int]) = proc {
		var prox: Int = primeiro
		val seed = new Random(System.currentTimeMillis())
		var res: Int = 0
		while (true) {
			println("#" + prox + " chegou no RU.")
			
			res = seed.nextInt(2)+1
			res match {
				case 1 =>
					println("#" + prox + " entrou na fila para comprar o Tiquete")
					filaTiq = filaTiq ++ List(prox)
				case 2 =>
					println("#" + prox + " desistiu da fila")
			}

			prox = proxEstudante(prox)
		}
	}

	/* Avança os estudantes na fila sincronizando com o processo 'caixa'.
	*  - filaTiq: lista compartilhada representando os estudantes que estão na fila do Tiquete.
	*  - filaEnt lista compartilhada representando os estudantes que estão na fila de Entrada. 
	*  - entrarCaixa, comprarTiquete, sairCaixa: canais de sic. com o caixa. */
	def filaTiquete(filaTiq: MutableList[Int], filaEnt: MutableList[Int], entrarCaixa: ?[Int], comprarTiquete: ![Unit], sairCaixa: ![Unit]) = proc {
	}

	/* Avança os estudantes na fila sincronizando com o processo 'filaTiquete'.
	*  - i: id do caixa.
	*  - filaTiq: lista compartilhada representando os estudantes que estão na fila do Tiquete.
	*  - filaEnt lista compartilhada representando os estudantes que estão na fila de Entrada. 
	*  - entrarCaixa, comprarTiquete, sairCaixa: canais de sic. com o caixa. */
	def caixa(i: Int, filaTiq: List[Int], entrarCaixa: ![Int], comprarTiquete: ?[Unit], sairCaixa: ?[Unit]) {
	}

	def main(args: Array[String]) {
		// Declarar canais...
		// Chamar o processo principal...
 		exit
	}
} 