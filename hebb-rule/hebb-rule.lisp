;; Funções de separação do w e b
(defun w-separated (pesos-ajustados)
  (butlast pesos-ajustados))
(defun b-separated (pesos-ajustados)
 (last pesos-ajustados))

;; Ajuste dos pesos, retorna o novo peso
(defun ajuste-peso (entradas pesos saida)
  (map 'list #'+ pesos
       (map 'list #'(lambda (x) (* x saida)) entradas)))

;; Definição da função treinamento
(defun treinamento (entradas pesos saidas)
  (if (and (null entradas) (null  saidas)) pesos
      (treinamento
       (rest entradas)
       (ajuste-peso (first entradas) pesos (first saidas))
       (rest saidas))))

;; Definição da função que define a saída
;; List, list -> Number
(defun soma (pesos-ajustados entrada)
  (+ (first (b-separated pesos-ajustados))
     (apply #'+ (map 'list  #'* (w-separated pesos-ajustados) (w-separated entrada)))))

;; Definição da função que testa se a saída é ativada ou não
(defun degrau (entrada pesos-ajustados limiar)
  (if (>= (soma pesos-ajustados entrada) limiar) 1 -1))

;; Função que aplica degrau pra todas as entradas
(defun compara-saidas (entradas pesos saidas limiar)
  (compara-saidas-aux entradas (treinamento entradas pesos saidas) saidas limiar '()))
(defun compara-saidas-aux (entradas pesos saidas limiar resultado)
  (cond
    ((null entradas) (format t "Pesos: [~{~a~^ ~}] || b: ~a ~%"
			     (w-separated pesos)
			     (first (b-separated pesos))))
    (t
     (format t "Saída esperada:  ~a || Saída obtida: ~a ~%"
	     (first saidas)
	     (degrau (first entradas) pesos limiar))
     (compara-saidas-aux (rest entradas) pesos (rest saidas) limiar
		       (append resultado
			       (list (degrau (first entradas) pesos limiar)))))))
