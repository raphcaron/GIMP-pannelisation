(define (script-fu-nouveau-panneau cWidth cHeight nbColonnes nbLignes nbTabsWidth nbTabsHeight tabWidth scrapsWidth routingWidth isGrooves groovesWidth)

  (let*(
      ;Calculs de distance du rebord du premier et dernier tabs horizontal et vertical
      (distTabRebordH (- (/ cWidth (+ nbTabsWidth 1)) (/ tabWidth 2)))
      (distTabRebordV (- (/ cHeight (+ nbTabsHeight 1)) (/ tabWidth 2)))

      ;Largeur de l'image
      (theImageWidth  (+ (* routingWidth (+ nbColonnes 1)) (* nbColonnes cWidth) (* 2 scrapsWidth)))

      ;Hauteur de l'image
      (theImageHeight (+ (* routingWidth (+ nbLignes 1)) (* nbLignes cHeight) (* 2 scrapsWidth)))

      ;Élément Image, qui recevera tous les calques
      (theImage (car (gimp-image-new theImageWidth theImageHeight RGB)))

      ;Calque d'un circuit
      (unCircuitTemplate (car (gimp-layer-new theImage cWidth cHeight RGB-IMAGE "Circuit" 100 0)))

      ;Calque d'un tab vertical
      (unTabVerticalTemplate (car (gimp-layer-new theImage routingWidth tabWidth RGB-IMAGE "TabV" 100 0)))

      ;Calque d'un tab horizontal
      (unTabHorizontalTemplate (car (gimp-layer-new theImage tabWidth routingWidth RGB-IMAGE "TabH" 100 0)))

      ;Calque d'un scrap edge vertical
      (unScrapVertical (car (gimp-layer-new theImage scrapsWidth theImageHeight RGB-IMAGE "Scrap Left" 100 0)))

      ;Calque d'un scrap edge horizontal
      (unScrapHorizontal (car (gimp-layer-new theImage theImageWidth scrapsWidth RGB-IMAGE "Scrap Top" 100 0)))

      ;Calque d'un groove vertical
      (grooveVerticalTemplate (car (gimp-layer-new theImage groovesWidth theImageHeight RGB-IMAGE "Groove Vertical" 100 0)))

      ;Calque d'un groove horizontal
      (grooveHorizontalTemplate (car (gimp-layer-new theImage (- theImageWidth (* 2 scrapsWidth)) groovesWidth RGB-IMAGE "Groove Horizontal" 100 0)))

      ;Background de toute l'image
      (theBackground (car (gimp-layer-new theImage theImageWidth theImageHeight RGB-IMAGE "BackgroundG" 100 0)))

      ;Variables générales
      (tabHorizontalTranslate 0)
      (tabVerticalTranslate 0)
      (stepTabsHorizontaux 0)
      (stepTabsVerticaux 0)

      ;Variables d'incrémentation
      (incrementColonne 0)
      (incrementLigne 0)
      (incrementTab 0)
    );fin des variables

    ;Calcule la distance de step si plusieurs tabs
    (if (<> nbTabsWidth 1)
      (set! stepTabsHorizontaux (/ (- cWidth tabWidth (* 2 distTabRebordH)) (- nbTabsWidth 1)))
    )

    (if (<> nbTabsHeight 1)
      (set! stepTabsVerticaux (/ (- cHeight tabWidth (* 2 distTabRebordV)) (- nbTabsHeight 1)))
    )

    ;Création des différents groupes dans lesquels placer les layers
    ;Création d'un layer "groupe"
    (define Resultat (car (gimp-layer-group-new theImage)))
    ;Assignation d'un nom
    (gimp-item-set-name Resultat "Resultat")
    ;Insertion du groupe dans l'image
    (gimp-image-insert-layer theImage Resultat 0 0)

    (define Tabs_Tous (car (gimp-layer-group-new theImage)))
    (gimp-item-set-name Tabs_Tous "Tabs_Tous")
    (gimp-image-insert-layer theImage Tabs_Tous Resultat 0)

    (define Tabs_Verticaux (car (gimp-layer-group-new theImage)))
    (gimp-item-set-name Tabs_Verticaux "Tabs_Verticaux")
    (gimp-image-insert-layer theImage Tabs_Verticaux Tabs_Tous 0)

    (define Tabs_Horizontaux (car (gimp-layer-group-new theImage)))
    (gimp-item-set-name Tabs_Horizontaux "Tabs_Horizontaux")
    (gimp-image-insert-layer theImage Tabs_Horizontaux Tabs_Tous 0)

    (define Scraps (car (gimp-layer-group-new theImage)))
    (gimp-item-set-name Scraps "Scraps")
    (gimp-image-insert-layer theImage Scraps Resultat 0)

    (define Circuits (car (gimp-layer-group-new theImage)))
    (gimp-item-set-name Circuits "Circuits")
    (gimp-image-insert-layer theImage Circuits Resultat 0)

    (define Grooves (car (gimp-layer-group-new theImage)))
    (gimp-item-set-name Grooves "Grooves")
    (gimp-image-insert-layer theImage Grooves Resultat 0)

    ;Insert les layers de bases dans l'image et dans leur groupe
    (gimp-image-insert-layer theImage theBackground Resultat 99)
    (gimp-image-insert-layer theImage unScrapVertical Scraps 0)
    (gimp-image-insert-layer theImage unScrapHorizontal Scraps 0)
    (gimp-image-insert-layer theImage unCircuitTemplate Circuits 0)
    (gimp-image-insert-layer theImage unTabVerticalTemplate Tabs_Verticaux 0)
    (gimp-image-insert-layer theImage unTabHorizontalTemplate Tabs_Horizontaux 0)

    ;Colorise les layers
    (gimp-context-set-background '(255 255 255)) ;RGB Blanc
    (gimp-drawable-fill theBackground BACKGROUND-FILL)
    (gimp-context-set-foreground '(6 108 184)) ;RGB Bleu slate
    (gimp-drawable-fill unCircuitTemplate FOREGROUND-FILL)
    (gimp-context-set-foreground '(8 129 0)) ;RGB Vert foret
    (gimp-drawable-fill unTabVerticalTemplate FOREGROUND-FILL)
    (gimp-drawable-fill unTabHorizontalTemplate FOREGROUND-FILL)
    (gimp-drawable-fill unScrapVertical FOREGROUND-FILL)
    (gimp-drawable-fill unScrapHorizontal FOREGROUND-FILL)

    ;Repositionne les layers en X Y
    (gimp-layer-translate unCircuitTemplate (+ scrapsWidth routingWidth) (+ scrapsWidth routingWidth))
    (gimp-layer-translate unTabVerticalTemplate scrapsWidth 0)
    (gimp-layer-translate unTabHorizontalTemplate 0 scrapsWidth)

    ;Duplique et repositionne les scrap edges pour faire le contour
    (define new_layer (car (gimp-layer-copy unScrapVertical 0)))
    (gimp-item-set-name new_layer "ScrapV")
    (gimp-layer-translate new_layer (- theImageWidth scrapsWidth) 0)
    (gimp-image-insert-layer theImage new_layer Scraps 0)

    (define new_layer (car (gimp-layer-copy unScrapHorizontal 0)))
    (gimp-item-set-name new_layer "ScrapH")
    (gimp-layer-translate new_layer 0 (- theImageHeight scrapsWidth))
    (gimp-image-insert-layer theImage new_layer Scraps 0)

    ;Incrémente le nombre de colonnes et le nombre de lignes
    (while (<= incrementLigne nbLignes)
      (while (<= incrementColonne nbColonnes)

        ;Incrémente le nombre de tabs par cicuit
        (while (< incrementTab nbTabsHeight)

          ;Calcule les coordonnées d'un tab vertical pour ce circuit en particulier
          (set! tabVerticalTranslate (+ scrapsWidth (* routingWidth (+ incrementLigne 1)) distTabRebordV (* cHeight incrementLigne) (* stepTabsVerticaux incrementTab)))

          ;Crée le tab, le positionne et l'insère à l'image
          (define new_layer (car (gimp-layer-copy unTabVerticalTemplate 0)))
          (gimp-item-set-name new_layer "TabV")
          (gimp-layer-translate new_layer (* incrementColonne (+ cWidth routingWidth)) tabVerticalTranslate)
          (gimp-image-insert-layer theImage new_layer Tabs_Verticaux 0)

          ;Incrémente d'un tab
          (set! incrementTab (+ incrementTab 1))

        );fin des tabs verticaux d'un circuit
        (set! incrementTab 0)

        ;Début des tabs horizontaux pour ce circuit
        (while (< incrementTab nbTabsWidth)

          ;Calcule les coordonnées d'un tab horizontal pour ce circuit en particulier
          (set! tabHorizontalTranslate (+ scrapsWidth (* routingWidth (+ incrementColonne 1)) distTabRebordH (* cWidth incrementColonne) (* stepTabsHorizontaux incrementTab)))

          ;Crée le tab, le positionne et l'insère à l'image
          (define new_layer (car (gimp-layer-copy unTabHorizontalTemplate 0)))
          (gimp-item-set-name new_layer "TabH")
          (gimp-layer-translate new_layer tabHorizontalTranslate (* incrementLigne (+ cHeight routingWidth)))
          (gimp-image-insert-layer theImage new_layer Tabs_Horizontaux 0)

          (set! incrementTab (+ incrementTab 1))

        );Fin des tabs horizontaux d'un circuit
        (set! incrementTab 0)

        ;Création d'un nouveau circuit à une position de colonne et ligne particulière, seulement si on n'est pas à la dernière incrémentation.
        (if (< incrementLigne nbLignes)
          (if (< incrementColonne nbColonnes)
            (begin
              (define new_layer (car (gimp-layer-copy unCircuitTemplate 0)))
              (gimp-item-set-name new_layer "Circuit")
              (gimp-layer-translate new_layer (* incrementColonne (+ cWidth routingWidth)) (* incrementLigne (+ cHeight routingWidth)))
              (gimp-image-insert-layer theImage new_layer Circuits 0)
            )
          )
        )
        (set! incrementColonne (+ incrementColonne 1))
      );Fin d'une ligne complète avec toutes ses colonnes
      (set! incrementColonne 0)

      (set! incrementLigne (+ incrementLigne 1))
    );Fin de toutes les lignes et toutes les colonnes
    (set! incrementLigne 0)

    ;Affichage des grooves
    (if (= isGrooves 1)
      (begin
        ;Insère les grooves dans l'image et les colorise
        (gimp-image-insert-layer theImage grooveVerticalTemplate Grooves 0)
        (gimp-image-insert-layer theImage grooveHorizontalTemplate Grooves 0)
        (gimp-context-set-background '(204 0 0) ) ;RGB Dark red
        (gimp-drawable-fill grooveVerticalTemplate BACKGROUND-FILL)
        (gimp-drawable-fill grooveHorizontalTemplate BACKGROUND-FILL)

        ;Incrémente le nombre de colonnes pour placer les grooves
        (while (< incrementColonne nbColonnes)
          ;Duplique et repositionne les grooves sur chaque circuit
          (define new_layer (car (gimp-layer-copy grooveVerticalTemplate 0)))
          (gimp-item-set-name new_layer "Groove Vertical")
          (gimp-layer-translate new_layer (- (+ scrapsWidth (* (+ incrementColonne 1) routingWidth) (* incrementColonne cWidth)) (/ groovesWidth 2)) 0)
          (gimp-image-insert-layer theImage new_layer Grooves 0)

          (define new_layer (car (gimp-layer-copy grooveVerticalTemplate 0)))
          (gimp-item-set-name new_layer "Groove Vertical")
          (gimp-layer-translate new_layer (- (+ scrapsWidth (* (+ cWidth routingWidth) (+ incrementColonne 1))) (/ groovesWidth 2)) 0)
          (gimp-image-insert-layer theImage new_layer Grooves 0)

          (set! incrementColonne (+ incrementColonne 1))
        )

        (while (< incrementLigne nbLignes)
          ;Duplique et repositionne les grooves sur chaque circuit
          (define new_layer (car (gimp-layer-copy grooveHorizontalTemplate 0)))
          (gimp-item-set-name new_layer "Groove Horizontal")
          (gimp-layer-translate new_layer scrapsWidth (- (+ scrapsWidth (* (+ incrementLigne 1) routingWidth) (* incrementLigne cHeight)) (/ groovesWidth 2)))
          (gimp-image-insert-layer theImage new_layer Grooves 0)

          (define new_layer (car (gimp-layer-copy grooveHorizontalTemplate 0)))
          (gimp-item-set-name new_layer "Groove Horizontal")
          (gimp-layer-translate new_layer scrapsWidth (- (+ scrapsWidth (* (+ cHeight routingWidth) (+ incrementLigne 1))) (/ groovesWidth 2)))
          (gimp-image-insert-layer theImage new_layer Grooves 0)

          (set! incrementLigne (+ incrementLigne 1))
        )
        (gimp-image-remove-layer theImage grooveVerticalTemplate)
        (gimp-image-remove-layer theImage grooveHorizontalTemplate)
      )
    )

    ;Nettoie les restes de templates
    (gimp-image-remove-layer theImage unTabVerticalTemplate)
    (gimp-image-remove-layer theImage unTabHorizontalTemplate)
    (gimp-image-remove-layer theImage unCircuitTemplate)

    ;Merge et affiche l'image résultante
    (gimp-layer-resize-to-image-size Resultat)
    (gimp-image-merge-visible-layers theImage 0)
    (gimp-context-set-background '(255 255 255))
    (gimp-context-set-foreground '(0 0 0))
    (gimp-display-new theImage)
    (gimp-image-clean-all theImage)
  )
)

;Enregistre les informations du script dans GIMP
(script-fu-register
  "script-fu-nouveau-panneau"       ;Nom de la fonction
  "Nouveau Panneau"                 ;Label dans le menu
  "Nouveau Panneau"                 ;Description
  "Raphael Caron"                   ;Auteur
  "Copyright 2019, Raphael Caron;\
  2019, C-MAC"                      ;Copyright
  "december 18, 2019"               ;Date, c'est ma fête!
  ""                                ;Image sur laquelle le script fonctionne (Vide: nouvelle image)

  ;Liste des informations demandées dans le popup au départ de la fonction
  ;Input ajustable "Label à afficher" '(Valeur_défault Min Max Incrément_Clic Incrément_PgUp/Dn Nb_Digits 0:Slider ou 1:Flèches)
  SF-ADJUSTMENT  "Largeur d'un circuit (mils)"        '(1500 1 20000 1 10 0 0)
  SF-ADJUSTMENT  "Hauteur d'un circuit (mils)"        '(1000 1 20000 1 10 0 0)
  SF-ADJUSTMENT  "Nb de colonnes"                     '(2 1 250 1 10 0 1)
  SF-ADJUSTMENT  "Nb de lignes"                       '(2 1 250 1 10 0 1)
  SF-ADJUSTMENT  "Nb de tabs/circuit sur la largeur"  '(3 0 100 1 1 0 1)
  SF-ADJUSTMENT  "Nb de tabs/circuit sur la hauteur"  '(2 0 100 1 1 0 1)
  SF-ADJUSTMENT  "Largeur des tabs (Défaut: 200 mils)"            '(200 1 1000 1 10 0 0)
  SF-ADJUSTMENT  "Largeur des scrap edges (Défaut: 300 mils)"     '(300 1 1000 1 10 0 0)
  SF-ADJUSTMENT  "Largeur du routing (Défaut: 95 mils)"          '(95 1 1000 1 10 0 1)
  SF-TOGGLE      "Grooves"                            TRUE
  SF-ADJUSTMENT  "Largeur des grooves (pixels)"       '(25 1 1000 1 1 0 1)
)

(script-fu-menu-register "script-fu-nouveau-panneau" "<Image>/File/Create")
