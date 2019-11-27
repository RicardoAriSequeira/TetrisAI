;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                        ;
;                           Instituto Superior Tecnico - Taguspark                       ;
;                                 1. Semestre 2015/2016                                  ;
;               Projeto da Cadeira de Inteligencia Artificial - IA Tetris                ;
;                                        Grupo 35                                        ;
;                                Bernardo Eichler (77988)                                ;
;                                   Pedro Bras (79767)                                   ;
;                                Ricardo Sequeira (79750)                                ;
;                                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TIPO ACCAO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Funcao que cria uma acao ao criar uma lista de dois elementos em que o primeiro e a posicao
;; da peca (coluna mais a esquerda), e o segundo elemento e a configuracao da peca.
(defun cria-accao( posicao configuracao )
    (cons posicao configuracao)
)

;; Funcao que devolve o primeiro elemento da acao, ou seja, a posicao da peca (coluna mais a esquerda).
(defun accao-coluna( accao )
    (first accao)
)

;; Funcao que devolve o segundo elemento da acao, ou seja, a configuracao da peca.
(defun accao-peca( accao )
    (cdr accao)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TIPO TABULEIRO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Funcao que cria um tabuleiro, que consiste num array em que cada elemento e uma lista de 
;; booleanos para cada coluna sinalizando se essa posicao esta ocupada ou nao por uma peca.
(defun cria-tabuleiro()
    (make-array '(18 10) :initial-contents (make-list 18 :initial-element (make-list 10))))

;; Funcao auxiliar para copia-tabuleiro que transforma array em lista.
(defun 2d-array-to-list (array)

  (loop for i below (array-dimension array 0)

        collect (loop for j below (array-dimension array 1)

                      collect (aref array i j))))

;; Funcao que devolve uma copia do tabuleiro que foi passado como argumento.
(defun copia-tabuleiro( tabuleiro )

  (cond ((equal tabuleiro nil) nil)

        (t (let ((lista ( 2d-array-to-list tabuleiro)))

                (make-array '(18 10)  :initial-contents lista))))
)

;; Funcao responsavel que recebe um tabuleiro, linha e coluna e verifica se esse mesmo tabuleiro
;; tem a posicao especificada pela linha e coluna ocupada.
(defun tabuleiro-preenchido-p(tabuleiro n-linha n-coluna)
    (aref tabuleiro (- 17 n-linha) n-coluna))

;; Funcao que permite saber a altura de uma certa coluna do tabuleiro especificado.
(defun tabuleiro-altura-coluna(tabuleiro n-coluna)

  (let ((i 17))
    
    (loop while(and (> i -1) (not(tabuleiro-preenchido-p tabuleiro i n-coluna)))
        do(setf i (- i 1)))

    (+ i 1)
))

;; Funcao que verifica se a linha especificada esta completamente preenchida no tabuleiro.
(defun tabuleiro-linha-completa-p(tabuleiro n-linha)

  (let ((x t)(i 0))

    (loop while(and (< i 10) (eq x t))
        do(if (not(tabuleiro-preenchido-p tabuleiro n-linha i))
                (setf x nil) nil)
        (setf i (+ i 1)))
    x
))

;; Funcao que permite preencher determinada posicao do tabuleiro (colocando a respetiva a true).
(defun tabuleiro-preenche!(tabuleiro linha coluna)

  (if (and (<= 0 linha) (<= linha 17)(<= 0 coluna)(<= coluna 9))
           (setf(aref tabuleiro (- 17 linha) coluna ) T))
)

;; Funcao que permite remover uma linha do tabuleiro, ao colocar todas essas posicoes a NIL.
(defun tabuleiro-remove-linha!(tabuleiro n-linha)

  (loop for i from (+ n-linha 1) to 18 

    do(loop for j below 10

      do(if (< i 18) (setf (aref tabuleiro (- 17 (- i 1)) j) (aref tabuleiro (- 17 i) j)))

        (if (> i 16) (setf (aref tabuleiro 0 j) nil))))
)

;; Funcao que permite saber se o topo do tabuleiro esta preenchido
(defun tabuleiro-topo-preenchido-p(tabuleiro)

  (let ((x nil))

    (loop for aux below 10 
       do (if (equal (tabuleiro-altura-coluna tabuleiro aux) 18)
          (setf x t))) 
    x 
))

;; Funcao que permite saber se dois tabuleiros sao iguais
(defun tabuleiros-iguais-p(tabuleiro tabuleiro1)

  (let ((x t))
  
  (loop for i below 18
      do (loop for j below 10
      do (if (not (equal (tabuleiro-preenchido-p tabuleiro i j)(tabuleiro-preenchido-p tabuleiro1 i j)))
          (setf x nil))))
    x
))

;; Funcao que devolve um array correspondente ao tabuleiro recebido
(defun tabuleiro->array(tabuleiro)

   (let ((lista (2d-array-to-list tabuleiro))
          (l '()))

   (loop for i below 18
    do (setf l (cons (nth i lista) l)))

   (make-array '(18 10)  :initial-contents l))
)

;; Funcao que devolve um tabuleiro correspondente ao array recebido
(defun array->tabuleiro(array)
  (tabuleiro->array array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TIPO ESTADO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct estado
                pontos
                pecas-por-colocar
                pecas-colocadas
                tabuleiro
)

;; Funcao que permite copiar um estado recebido
(defun copia-estado (e) 

  (let ((novas-pecas-por-colocar (copy-list (estado-pecas-por-colocar e)))
        (novas-pecas-colocadas (copy-list (estado-pecas-colocadas e))))

      (make-estado :pontos (estado-pontos e)
                   :pecas-por-colocar novas-pecas-por-colocar
                   :pecas-colocadas novas-pecas-colocadas
                   :tabuleiro (copia-tabuleiro (estado-tabuleiro e)))             
))

;; Funcao que compara dois estados, verificando se sao iguais
(defun estados-iguais-p(e1 e2)

  ( and (= (estado-pontos e1) (estado-pontos e2))
            (equal (estado-pecas-por-colocar e1) (estado-pecas-por-colocar e2))
            (equal (estado-pecas-colocadas e1) (estado-pecas-colocadas e2))
            (tabuleiros-iguais-p (estado-tabuleiro e1) (estado-tabuleiro e2)))

)

;; Funcao que identifica o estado final como estado terminal
(defun estado-final-p(e)
  (or (tabuleiro-topo-preenchido-p (estado-tabuleiro e)) (null (estado-pecas-por-colocar e)) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TIPO PROBLEMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct problema
  estado-inicial
  solucao
  accoes
  resultado
  custo-caminho
)

;; Funcao que identifica o estado como solucao
(defun solucao(e)
  (and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro e)) ) (estado-final-p e) )
)

;; Funcao que devolve as acoes fisicamente possiveis de efetuar de acordo com o estado recebido
(defun accoes(e)

  (let (( pecas (simbolo->pecas (first (estado-pecas-por-colocar e))) )
        ( lista-acoes '() ))

    (loop for peca in pecas

      do (let (( dimensao-horizontal (second (array-dimensions peca ))))

           (if (not (tabuleiro-topo-preenchido-p (estado-tabuleiro e))) 

               (loop for i below 10

                 do (if (< (+ i dimensao-horizontal) 11)

                        (setf lista-acoes (append lista-acoes (list (cria-accao i peca))))))))
    )

    lista-acoes
))


;; Funcao que devolve o estado resultante do estado recebido, efetuando a acao recebida
(defun resultado(est acc)

  (let( ; lista de pecas por colocar para o estado resultante
        (lista-pecas-por-colocar (cdr (estado-pecas-por-colocar est)))

        ; lista de pecas colocadas para o estado resultante
        (lista-pecas-colocadas (append (list (first(estado-pecas-por-colocar est))) (estado-pecas-colocadas est)))

        ; novo tabuleiro para o estado resultante
        (novo-tabuleiro (copia-tabuleiro (estado-tabuleiro est)))

        ; variavel dos pontos do estado resultante
        (novos-pontos (estado-pontos est))

        ; numero de linhas verificadas pela funcao
        (count 0)

        ; numero de linhas completas nesta jogada
        (l 0)

        ; estado resultante
        (novo-estado nil))


    ; colocamos a peca no novo tabuleiro de acordo com a acao recebida
    (setf novo-tabuleiro (coloca-peca acc novo-tabuleiro))
    
    ; verificamos se o topo do tabuleiro esta preenchido
    (if (not (tabuleiro-topo-preenchido-p novo-tabuleiro)) 

        ; vamos verificar se existe alguma linha completa no tabuleiro, para alem do topo
        (loop while (< count 17)

          ; se a linha estiver completa
          do (cond ( (tabuleiro-linha-completa-p novo-tabuleiro count)

                    ; removemos a linha
                    (tabuleiro-remove-linha! novo-tabuleiro count)

                    ; incrementa o numero de linhas completas nesta jogada
                    (setf l (+ l 1)))

                   ; se a linha nao tiver completa avanca para a proxima, se nao, continua na mesma (porque desceu)
                   (t (setf count (+ count 1))))

          
          ) )

    ; soma pontos ao estado resultante de acordo com as linhas completas nesta jogada
    (setf novos-pontos (+ novos-pontos (atribuir-pontos l)))

    ; cria o estado resultante com os parametros calculados durante esta funcao
    (setf novo-estado (make-estado :pontos novos-pontos
               :pecas-por-colocar lista-pecas-por-colocar
               :pecas-colocadas lista-pecas-colocadas
               :tabuleiro novo-tabuleiro))

    novo-estado

   )
)

;; Funcao que devolve a qualidade do estado recebido
(defun qualidade(e)
  (- (estado-pontos e))
)

;; Funcao que devolve o custo-oportunidade do estado recebido
(defun custo-oportunidade(e)

  (let((M (pontuacao-maxima (estado-pecas-colocadas e))))

    (- M (estado-pontos e))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; ESTRUTURA PARA ALGORITMOS DE PROCURA ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct estado-f
  estado
  valor
  acoes-tomadas
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROCURA EM PROFUNDIDADE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun procura-pp (p)

  (let* ((lista-acoes-tomadas '())
         (estado-atual (make-estado-f :estado (problema-estado-inicial p) :valor 0 :acoes-tomadas '()))
         (estado-resultante nil)
         (lista-acoes-estado-atual (funcall (problema-accoes p) (estado-f-estado estado-atual)))
         (lista-estados-por-explorar (list estado-atual)))

   ; O algoritmo apenas para quando o estado atual for solucao ou nao existir mais estados para explorar
   (loop while (and (not (funcall (problema-solucao p) (estado-f-estado estado-atual)))
                    (not (eq lista-estados-por-explorar nil)))

          ; Retiramos um estado da lista-estados-por-explorar e atualizamos as listas dependentes
          do (setf estado-atual (pop lista-estados-por-explorar))
          (setf lista-acoes-estado-atual (funcall (problema-accoes p) (estado-f-estado estado-atual)))
          (setf lista-acoes-tomadas (estado-f-acoes-tomadas estado-atual))

          ; Adicionamos estados a fronteira
           (loop for acao in lista-acoes-estado-atual
      
               ; Analisamos os estados resultantes das acoes sobre o estado atual
               do (setf estado-resultante (funcall (problema-resultado p) (estado-f-estado estado-atual) acao) )

                  ; Se o estado tiver acoes possiveis ou for solucao, adicionamos a lista de estados
                  (if (or (funcall (problema-solucao p) estado-resultante)
                          (not (eq (funcall (problema-accoes p) estado-resultante) nil)))

                              (progn
                                (setf estado-resultante (make-estado-f :estado estado-resultante
                                                                       :valor 0
                                                                       :acoes-tomadas (append lista-acoes-tomadas (list acao))))
                                (setf lista-estados-por-explorar (append (list estado-resultante) lista-estados-por-explorar))
                              )
                  )
           )
    )

    lista-acoes-tomadas
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROCURA A* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun procura-A* (p heuristica)

  (let* ((estado-atual (make-estado-f :estado (problema-estado-inicial p) :valor 0 :acoes-tomadas '()))
        (lista-estados-por-explorar (list estado-atual))
        (estado-aux nil)
        (index nil)
        (lista-acoes-possiveis (funcall (problema-accoes p) (estado-f-estado estado-atual)))
        (lista-acoes-tomadas '()))

   (setf estado-atual (estado-f-estado estado-atual))

   ; O algoritmo apenas para quando o estado atual for solucao ou nao existir mais estados para explorar
   (loop while (and (not (funcall (problema-solucao p) estado-atual))
                    (not (eq lista-estados-por-explorar nil)))

     ; Adicionamos estados a fronteira
      do (loop for acao in lista-acoes-possiveis
             
           do (setf estado-aux (funcall (problema-resultado p) estado-atual acao) )

              ; Apenas adiciona a fronteira se o estado tiver acoes possiveis ou for solucao
              (if (or (funcall (problema-solucao p) estado-aux)
                      (not (eq (funcall (problema-accoes p) estado-aux) nil)))
                  
                  ; Otimizacao nao totalmente correta : Corte na arvore de acordo com o valor do estado
                 (if (< (+ (funcall (problema-custo-caminho p) estado-aux)
                                                               (funcall heuristica estado-aux)) 2500 )
                     (progn
                     (setf estado-aux (make-estado-f :estado estado-aux
                                                     :valor (+ (funcall (problema-custo-caminho p) estado-aux)
                                                               (funcall heuristica estado-aux))
                                                     :acoes-tomadas (append lista-acoes-tomadas (list acao))))
                     
                     (setf lista-estados-por-explorar (append lista-estados-por-explorar (list estado-aux)))))))
          
          ; Chama a funcao responsavel por obter o valor minimo da lista de estados
          (setf index (valor-minimo lista-estados-por-explorar))

          ; Depois de saber o indice, escolhe o estado, retira-o da lista e atualiza variaveis
          (setf estado-atual (nth index lista-estados-por-explorar))
          (setf lista-acoes-tomadas (estado-f-acoes-tomadas estado-atual))
          (setf estado-atual (estado-f-estado estado-atual))
          (setf lista-acoes-possiveis (funcall (problema-accoes p) estado-atual))
          (setf lista-estados-por-explorar (remove-nth index lista-estados-por-explorar))
   )  

  lista-acoes-tomadas
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROCURA BEST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun procura-best (array lista-pecas)

  (let* ((estado-atual (make-estado :pontos 0
                                   :pecas-por-colocar lista-pecas
                                   :pecas-colocadas '()
                                   :tabuleiro (array->tabuleiro array)))

        (problema (make-problema :estado-inicial estado-atual
                                 :solucao #'solucao
                                 :accoes #'accoes
                                 :resultado #'resultado
                                 :custo-caminho #'custo-oportunidade)))
    
    (procura-A* problema #'heuristica-best)
))


;; Heuristica escolhida pelo grupo a ser utilizada pela procura-best
(defun heuristica-best(e)
  
  (let ((tab (estado-tabuleiro e)) (res 0))

  ; Coeficientes calculados ao longo de varias experiencias/testes
  (setf res (+ (* 500 (buracos tab))
               (* 1 (qualidade e))
               (* 10 (higher-slope tab))))

     ;(* 1 (total-slopes tab))
     ;(* 1 (espacos-preenchidos tab)))
  res
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCOES AUXILIARES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Funcao que devolve a lista de configuracoes da peca correspondente ao simbolo recebido
(defun simbolo->pecas (x)

    (cond ( (eq 'i x) (list peca-i0 peca-i1) )
        ( (eq 'l x) (list peca-l0 peca-l1 peca-l2 peca-l3) )
        ( (eq 'j x) (list peca-j0 peca-j1 peca-j2 peca-j3) )
        ( (eq 'o x) (list peca-o0) )
        ( (eq 's x) (list peca-s0 peca-s1) )
        ( (eq 'z x) (list peca-z0 peca-z1) )
        ( (eq 't x) (list peca-t0 peca-t1 peca-t2 peca-t3) )
        ( t nil) )
)

;; Funcao que devolve os pontos correspondentes as linhas removidas
(defun atribuir-pontos (linhas)

  (let ((new-pontos 0))

  (cond((equal linhas 0)(setf new-pontos 0))
       ((equal linhas 1)(setf new-pontos 100))
       ((equal linhas 2)(setf new-pontos 300))
       ((equal linhas 3)(setf new-pontos 500))
       ((equal linhas 4)(setf new-pontos 800)))

   new-pontos
))


;; Funcao auxiliar responsavel por colocar uma peca no tabuleiro
(defun coloca-peca (acao tab)

  (let*( ; numero da primeira coluna que vai ser ocupada pela peca
        (coluna (first acao))
         ; peca que vai ser colocada
        (peca (cdr acao))
        ; numero de linhas ocupadas pela peca
        (numero-linhas (first(array-dimensions peca)))
        ; numero de colunas ocupadas pela peca
        (numero-colunas (second(array-dimensions peca)))
        ; variaveis auxiliares para verificar posicoes preenchidas do tabuleiro
        (nlinha (- (coluna-mais-alta-peca tab coluna numero-colunas numero-linhas) numero-linhas))
        (subiu t))

   ; enquanto houver posicoes preenchidas onde se pretende colocar a peca, sobe se no tabuleiro
   (loop while (and subiu (< (+ nlinha numero-linhas) 18))

      ; inicializamos o loop com a variavel a nil
      do (setf subiu nil)

       ; verificamos todas as linhas da peca
       (loop for linha below numero-linhas while (not subiu)

         ; verificamos todas as colunas da linha da peca
         do (loop for quadricula below numero-colunas while (not subiu)

           ; se a peca ocupar uma posicao que ja ta ocupada no tabuleiro
           do (if (or (and (aref peca linha quadricula)
                           (tabuleiro-preenchido-p tab (+ nlinha linha) (+ coluna quadricula)))
                      (and (aref peca linha quadricula)
                           (not(tabuleiro-preenchido-p tab (+ nlinha linha) (+ coluna quadricula)))
                           (buraco-p tab (+ nlinha linha) (+ coluna quadricula))))

                  ; sobe-se a linha do tabuleiro onde vamos colocar a peca
                  (progn (setf nlinha (+ nlinha 1))
                  (setf subiu t))))))

    ; preenche linhas e colunas do tabuleiro onde a peca e colocada
    (loop for linha below numero-linhas

      do (loop for quadricula below numero-colunas

           do (if (aref peca linha quadricula) (tabuleiro-preenche! tab (+ nlinha linha) (+ coluna quadricula)))))

  )
  tab
)

;; Funcao responsavel por verificar qual a coluna mais alta entre as colunas que vao ser
;; ocupadas pela peca
(defun coluna-mais-alta-peca (tab coluna numero-colunas numero-linhas)

  (let ((altura numero-linhas))

  (loop for c below numero-colunas
     do (if (> (tabuleiro-altura-coluna tab (+ coluna c)) altura) (setf altura (tabuleiro-altura-coluna tab (+ coluna c)))))

  altura
))


;; Funcao que devolve o custo-oportunidade associado a cada peca
(defun custo-oportunidade-peca(x)

  (cond ( (eq 'i x) 800 )
        ( (eq 'l x) 500 )
        ( (eq 'j x) 500 )
        ( (eq 'o x) 300 )
        ( (eq 's x) 300 )
        ( (eq 'z x) 300 )
        ( (eq 't x) 300 )
        ( t nil) )
)

;; Funcao que devolve a pontuacao maxima possivel para uma lista de pecas recebidas
(defun pontuacao-maxima (l)

  (let((lista-pecas-colocadas '())
       (pontuacao-maxima 0))

    (setf lista-pecas-colocadas (append lista-pecas-colocadas (list(first l))))
    (setf lista-pecas-colocadas (append lista-pecas-colocadas (cdr l)))

    (loop while (not(eq lista-pecas-colocadas '()))

      do (setf pontuacao-maxima (+ pontuacao-maxima (custo-oportunidade-peca (pop lista-pecas-colocadas)))))
  
  pontuacao-maxima
))

;; Funcao que indica que a posicao referida no tabuleiro indicado e um buraco
(defun buraco-p (tab linha coluna)

  (let ((x nil))

    (loop for l from (+ linha 1) to 17
      do (if (tabuleiro-preenchido-p tab l coluna) (setf x t)))

    x
))



;; Funcao que calcula a diferenca de alturas entre a coluna recebida e a coluna seguinte
(defun slope (tab coluna)

  (let ((res 0))

    (setf res (- (tabuleiro-altura-coluna tab coluna) (tabuleiro-altura-coluna tab (+ coluna 1))))
    (if (< res 0) (setf res (* res -1)))

    res
))

;; Funcao que calcula qual o maior slope de um tabuleiro
(defun higher-slope (tab)

  (let ((res 0)(aux 0))

    (loop for coluna below 9
        do (setf aux (slope tab coluna))
            (if (>= aux res) (setf res aux)))
  res
))

;; Funcao que calcula o total de slopes de um tabuleiro
(defun total-slopes (tab)

  (let ((total 0))

    (loop for coluna below 9
        do (setf total (+ total (slope tab coluna))))

  total
))

;; Funcao auxiliar a procura-A* que calcula o indice do estado com valor minimo de uma lista de estados
(defun valor-minimo(lista-estados-f)

  (let ((minimo (estado-f-valor (first(last lista-estados-f))))
        (i 0)
        (indice-retirar (- (list-length lista-estados-f) 1)))

       (loop for estado in lista-estados-f
         
         do  (if (<= (estado-f-valor estado) minimo)
                (progn 
                  (setf minimo (estado-f-valor estado))
                  (setf indice-retirar i)))
             (setf i (+ i 1)))

    indice-retirar
))

;; Funcao que remove determinado elemento da lista de acordo com o seu indice
(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;; Funcao que devolve o numero de espacos preenchidos no tabuleiro recebido
(defun espacos-preenchidos (tab)

  (let ((x 0))

    (loop for i below 10
      do (loop for j below 18
        do (if (tabuleiro-preenchido-p tab j i)
               (setf x (+ x 1)))))
  x
))

;; Funcao que devolve o numero de buracos no tabuleiro recebido
(defun buracos(tab)

  (let ((j 17)(x 0)(z nil))

    (loop for i below 10
      do (loop while (> j -1)

        do (if (tabuleiro-preenchido-p tab j i) (setf z t))

           (if (and (not(tabuleiro-preenchido-p tab j i)) z )
               (setf x (+ x 1)))

           (setf j (- j 1)))

         (setf z nil)
         (setf j 17))
  x
))

;(load "utils.fas")