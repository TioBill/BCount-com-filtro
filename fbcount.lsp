;; Fazer uma LISP onde o funcionamento é igual ao BCOUNT, porem filtrando os blocos por somente os que interessa
;; e de maneira ORGANIZADA.

;; Nomeclatura
;; C - Curvas | TE - Te | LCR - Luva | CZ - Cruzeta | RD - Redução | AD - Adaptador 

;; Organizar todos os BLOCO com nome C XXº, na categoria curvas; Com nome RD, na categoria Redução, etc...
(vl-load-com)

(defun c:FBCount ( / *error* longestString blocos totais curvas tes 
                  cruzetas reducoes adaptadores luvas
                  blocksSelected sum outros currentBlockText value

                  totais_sorted curvas_sorted tes_sorted luvas_sorted
                  cruzetas_sorted reducoes_sorted adaptadores_sorted
                  outros_sorted
                  
                  ;; CONSTANTS
                  CURVA_REGEX TE_REGEX LUVA_REGEX CRUZETA_REGEX OUTROS_REGEX
                  REDUCAO_REGEX ADAPTADOR_REGEX CURVA_TITLE TE_TITLE LUVA_TITLE CRUZETA_TITLE REDUCAO_TITLE ADAPTADOR_TITLE 
                      
                  ;; FUNCOES
                  substList sortedStrings drawTwoLinesNText regex
                )

  ;; Incrementa o valor de um item por um
  (defun substList ( item lista )
    (subst
      ;; Valor novo
      (cons item (+ (cdr (assoc item lista)) 1))
      
      ;; Valor antigo
      (assoc item lista)
      
      ;; Lista
      lista
    )
  )
  
  ;; Organiza a lista de maneira alfabetica
  (defun sortedStrings (lista / sorted currentItem)
    (setq sorted (list))
    
    (foreach currentItem
      lista
      (setq sorted (append sorted (list (car currentItem))))
    )
    
    (acad_strlsort sorted)
  )
  
  ;; Faz uma formatação, desenhando uma linha de "=", coloca um texto no meio, e abaixo faz uma nova linha de "="
  (defun drawTwoLinesNText (string longestString)
    (repeat (fix (/ (- longestString (strlen string)) 2)) (princ "="))
    (princ string)
    (repeat (fix (/ (- longestString (strlen string)) 2)) (princ "="))
  )
  
  ;|
    Regex
    OPTS:
    
    Global: False
    Multiline: True
    ignorecase: True
  |;
  (defun regex ( string pattern / rx result)
    (setq result nil)
    
    (if (setq rx (vlax-get-or-create-object "VBScript.RegExp"))
      (progn
        (setq result 
          (vl-catch-all-apply
            (function
              (lambda ()
                (vlax-put-property rx 'global acfalse)
                (vlax-put-property rx 'multiline actrue)
                (vlax-put-property rx 'ignorecase actrue)
                (vlax-put-property rx 'pattern pattern)

                (setq result (= -1 (vlax-invoke rx 'test string)))
              )
            )
          )
        )
      )
    )

    (vlax-release-object rx)
    
    (if (null (vl-catch-all-error-p result))
      result
    )
  )
 
  (defun *error* (msg)
    (or 
      (wcmatch (strcase msg t) "*break,*cancel*,*exit*") 
      (alert (strcat "ERROR: " msg "**"))
    )
  )

  ;; Constantes, para o uso em seus REGEX
  (setq 
    CURVA_REGEX "^C [0-9]+ [a-z-]+ D[NE] ?[0-9]+$"
    TE_REGEX "^TE (RD )?[a-z-]+ D[NE] ?[0-9]+( X D[NE] ?[0-9]+)?$"
    LUVA_REGEX "^LCR [a-z-]+ D[NE] ?[0-9]+$"
    CRUZETA_REGEX "^CZ (RD )?[a-z-]+ D[NE] ?[0-9]+( X D[NE] ?[0-9]+)?$"
    REDUCAO_REGEX "^RD [a-z-]+ D[NE] ?[0-9]+ X D[NE] ?[0-9]+$"
    ADAPTADOR_REGEX "^AD (RD )?[a-z-]+ X [a-z-]+ D[NE] ?[0-9]+( X D[NE] ?[0-9]+)?$"
    
    OUTROS_REGEX "^(C|TE|LCR|CZ|RD|AD) "
  )
  
  (setq
    CURVA_TITLE "^OSM CURVAS [0-9]+$"
    TE_TITLE "OSM TE"
    LUVA_TITLE "OSM LUVA"
    CRUZETA_TITLE "OSM CRUZETA"
    REDUCAO_TITLE "OSM REDUCAO"
    ADAPTADOR_TITLE "OSM ADAPTADOR"
  )

  (setq longestString 0)
  
  (setq blocos (ssget '((0 . "INSERT") (100 . "AcDbBlockReference"))))
  
  (setq
    ;; CONTEM TODAS AS LISTAS
    totais (list)
  
    ;; CONTEM TODAS AS CURVAS
    curvas (list)
    
    ;; CONTEM TODOS OS TEs
    tes (list)
    
    ;; CONTEM TODAS AS LUVAS
    luvas (list)
    
    ;; CONTEM TODAS AS CRUZETAS
    cruzetas (list)
    
    ;; CONTEM TODAS AS REDUÇÃO
    reducoes (list)
    
    ;; CONTEM TODOS OS ADAPTADORES
    adaptadores (list)
    
    
    ;; OUTROS ITEMS QUE NÃO PUDERAM SER ORGANIZADOS (Apenas peças que começam com C,TE,LCR,CZ,RD ou AD)
    outros (list)
  )
  
  (repeat (setq blocksSelected (sslength blocos))
    (setq currentBlock (vlax-ename->vla-object (ssname blocos (setq blocksSelected (1- blocksSelected)))))

    (setq currentBlockText (strcase (vla-get-name currentBlock)))
    
    (if 
      (cond 
        ;; Verifica se existe na lista  totais 
        ((assoc (substr currentBlockText 5) totais) ;; 5 para pular o nome OSM
          (setq currentBlockText (substr currentBlockText 5))
          (setq totais (substList currentBlockText totais))
          t 
        )

        ;; Verifica se existe na lista curvas
        ((assoc currentBlockText curvas)
          (setq curvas (substList currentBlockText curvas))
          t 
        )
        
        ;; Verifica se existe na lista tes
        ((assoc currentBlockText tes)
          (setq tes (substList currentBlockText tes))
          t 
        )

        ;; Verifica se existe na lista luvas 
        ((assoc currentBlockText luvas)
          (setq luvas (substList currentBlockText luvas))
          t 
        )

        ;; Verifica se existe na lista cruzeta 
        ((assoc currentBlockText cruzetas)
          (setq cruzetas (substList currentBlockText cruzetas))
          t 
        )

        ;; Verifica se existe na lista reducoes 
        ((assoc currentBlockText reducoes)
          (setq reducoes (substList currentBlockText reducoes))
          t 
        )

        ;; Verifica se existe na lista adaptador 
        ((assoc currentBlockText adaptadores)
          (setq adaptadores (substList currentBlockText adaptadores))
          t 
        )
        
        ;; Verifica se existe na lista outros
        ((assoc currentBlockText outros)
          (setq outros (substList currentBlockText outros)) 
        )

        
        ;; SEGUNDA SEÇÃO, CASO NÃO TENHA EXISTIDO EM NENHUMA DESSAS LISTAS
        ((or
          (regex currentBlockText CURVA_TITLE)
          (= currentBlockText TE_TITLE)
          (= currentBlockText LUVA_TITLE)
          (= currentBlockText CRUZETA_TITLE)
          (= currentBlockText REDUCAO_TITLE)
          (= currentBlockText ADAPTADOR_TITLE)
        )
         
          (setq currentBlockText (substr currentBlockText 5))
          (setq totais (append totais (list (cons currentBlockText 1)))) 
        
          t 
        )

        ;; CURVAS
        ((regex currentBlockText CURVA_REGEX) 
          (setq curvas (append curvas (list (cons currentBlockText 1)))) 
        
          t 
        )

        ;; TE OU TE REDUCAO
        ((regex currentBlockText TE_REGEX)
          (setq tes (append tes (list (cons currentBlockText 1)))) 
        
          t 
        )

        ;; LUVA
        ((regex currentBlockText LUVA_REGEX) 
          (setq luvas (append luvas (list (cons currentBlockText 1)))) 
        
          t 
        )

        ;; CRUZETA OU CRUZETA REDUCAO
        ((regex currentBlockText CRUZETA_REGEX)
          (setq cruzetas (append cruzetas (list (cons currentBlockText 1))))
        
          t 
        )

        ;; REDUCOES
        ((regex currentBlockText REDUCAO_REGEX)
          (setq reducoes (append reducoes (list (cons currentBlockText 1))))
        
          t 
        )

        ;; ADAPTADOR 
        ((regex currentBlockText ADAPTADOR_REGEX)
          (setq adaptadores (append adaptadores (list (cons currentBlockText 1))))
        
          t
        )
        
        ;| 
          Caso, não se encaixa em nenhum desses casos, verifique se o item começa com:
          
          "C " -> Possivel CURVA
          "TE " -> Possivel TE
          "LCR " -> Possivel LUVA
          "CZ " -> Possivel CRUZETA
          "RD " -> Possivel REDUÇÃO
          "AD " -> Possivel ADAPTADOR
          
          Nota, todos os textos, DEVEM TER (SIGLA) e um (ESPACO) logo após, para ser considerado nessa lista.
        |;
        
        ((regex currentBlockText OUTROS_REGEX)
          (setq outros (append outros (list (cons currentBlockText 1)))) 
        )
  
        (t nil)
      )
      (if (> (strlen currentBlockText) longestString) 
        (setq longestString (strlen currentBlockText))
      )
    )
  )
  
  (setvar 'cmdecho 0)
  
  (if (< longestString 26) (setq longestString 26))
  
  (setq longestString (+ 10 longestString))

  
  ;; LISTA ORGANIZADOS ALFABETICAMENTES
  (setq
    totais_sorted (sortedStrings totais)
    curvas_sorted (sortedStrings curvas)
    tes_sorted (sortedStrings tes)
    luvas_sorted (sortedStrings luvas)
    cruzetas_sorted (sortedStrings cruzetas)
    reducoes_sorted (sortedStrings reducoes)
    adaptadores_sorted (sortedStrings adaptadores)
    outros_sorted (sortedStrings outros)
  )
  
  (drawTwoLinesNText " RESULT " longestString)
  (prompt "TEXT")
  (repeat (- longestString 9) (princ "."))
  (princ "TOTAL")
  (terpri)
  
  (repeat longestString (princ "="))
  (terpri)

  (foreach currentList
    (list
      (cons " TOTAL " totais_sorted)
      (cons " CURVAS " curvas_sorted)
      (cons " TES " tes_sorted)
      (cons " LUVAS " luvas_sorted)
      (cons " CRUZETAS " cruzetas_sorted)
      (cons " REDUCAO " reducoes_sorted)
      (cons " ADAPTADORES " adaptadores_sorted)
      (cons " NAO CATEGORIZADO " outros_sorted)
    )
    
    (setq sum 0)
    
    (drawTwoLinesNText (car currentList) longestString)
    
    (mapcar
      (function
        (lambda (name)
          (prompt name)

          (setq value 0)
          
          (cond
            ((= (car currentList) " TOTAL ")
              (setq value (cdr (assoc name totais)))
            )
        
            ((= (car currentList) " CURVAS ")
              (setq value (cdr (assoc name curvas)))
            )
        
            ((= (car currentList) " TES ")
              (setq value (cdr (assoc name tes)))
            )
        
            ((= (car currentList) " LUVAS ")
              (setq value (cdr (assoc name luvas)))
            )
        
            ((= (car currentList) " CRUZETAS ")
              (setq value (cdr (assoc name cruzetas)))
            )
        
            ((= (car currentList) " REDUCAO ")
              (setq value (cdr (assoc name reducoes)))
            )
        
            ((= (car currentList) " ADAPTADORES ")
              (setq value (cdr (assoc name adaptadores)))
            )
        
            ((= (car currentList) " NAO CATEGORIZADO ")
              (setq value (cdr (assoc name outros)))
            )

            (t nil)
          )
          
          (repeat (- longestString (+ (strlen name) (strlen (itoa value))))
            (princ ".")
          )

          (princ value)
          
          (setq sum (+ sum value))
        )
      )
      
      (cdr currentList)
    )
    
    (terpri)
    (prompt "TOTAL DE PECAS ENCONTRADAS")
    (repeat (- longestString (+ 26 (strlen (itoa sum)))) (princ "."))
    (princ (itoa sum))
    (terpri)
  )

  (drawTwoLinesNText " ;) " longestString)
  
  (setvar 'cmdecho 1)
  
  (alert "Verifique a tabela em seu console! Pressione <F2> para uma melhor visualizacao, ou caso nao esteja visivel")
  (princ)
)



(alert "Lisp carregada com sucesso! Digite \"FBCount\" para comecar.")