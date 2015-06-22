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

	/* Adiciona estudantes à fila e os avança, sincronizando com o processo 'caixa'.
	*  - primeiro: id do primeiro estudante a chegar. 
	*  - comprarTiquete, sairCaixa: canais de sic. com o caixa. */
	def filaTiquete(primeiro: Int, comprarTiquete: Seq[![Int]], sairCaixa: Seq[?[Int]]) = proc {
		var prox: Int = primeiro
		var filaTiq = new List() // Lista representando os estudantes na fila
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
					filaTiq = filaTiq ++ [prox]
					println("#" + prox + " entrou na fila para comprar o Tiquete")
				case 2 =>
					println("#" + prox + " desistiu da fila")
			}
			prox = proxEstudante(prox)

			if (filaTiq.length > 0) {
				estudante = filaTiq.head

				print("#" + estudante + " digite o caixa em que quer comprar o tíquete: ")
				caixa = Console.readInt()

				comprarTiquete(caixa)!estudante;
				tiq = sairCaixa? ;
				println("#" + estudante + " comprou o tiquete para a catraca #" + tiq)

				filaTiq = filaTiq.tail // Atualiza a fila
			}

		}
	}

	/* Avança os estudantes na fila sincronizando com o processo 'filaTiquete'.
	*  - i: id do caixa.
	*  - comprarTiquete, sairCaixa: canais de sic. com o caixa. */
	def caixa(i: Int, comprarTiquete: ?[Int], sairCaixa: ![Int]) {
		var estudante: Int = 0
		while (true) {
			estudante = comprarTiquete? ;
			println("#" + estudante + "entrou no caixa #" + i)

			if (estudante%2 == 1) 
				sairCaixa!1
			else 
				sairCaixa!2
		}
	}

	def main(args: Array[String]) {
		// Declarar canais...
		// Chamar o processo principal...
 		exit
	}
} 