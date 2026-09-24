# Extension panel : périmètre et suites

La 0.14.0 candidate étend les panels linéaires statiques, dans l'API et Shiny.
Les corrections du socle 0.11.1 sont conservées. L'objectif reste la robustesse
et l'explication des hypothèses pour les chercheurs en sciences sociales.

## Inclus dans cette livraison

| Question | Réponse disponible |
|---|---|
| Structure des données | Index explicites, doublons, valeurs manquantes, équilibre, trous, singletons |
| Modèle linéaire statique | Pooled OLS, FE individuels, FE temporels, FE doubles, RE individuels, premières différences, between, Mundlak |
| Incertitude des coefficients | Covariance classique ou Arellano HC1 cluster individu, degrés de liberté affichés |
| Effets individuels/temporels | F classique FE contre pooled ; LM classique pour effets individuels |
| Dépendance des erreurs | BG panel et CD de Pesaran, avec conditions d'applicabilité |
| Hypothèse RE | Hausman classique ou auxiliaire robuste, à la demande, sans choix automatique |
| Traçabilité | Objets plm/lm, transformations, composantes Mundlak, covariances, lignes d'origine, exclusions, termes absorbés, échecs |

## Extensions à concevoir après validation du socle

| Famille | Modèles ou méthodes | Questions et tests associés |
|---|---|---|
| Inférence approfondie | Cluster double, CR2, wild cluster bootstrap, Driscoll-Kraay | Niveau d'assignation, nombre de clusters, dépendance et régime asymptotique |
| Erreurs et spécification | Tests de Wooldridge, variantes robustes des comparaisons d'effets | Hétéroscédasticité, autocorrélation, spécification ; aucun « meilleur modèle » automatique |
| Endogénéité | Panel IV/2SLS, Hausman-Taylor | Identification, force des instruments, restrictions d'exclusion, suridentification |
| Dynamiques | Différence-GMM et système-GMM | AR(1)/AR(2), Sargan/Hansen, instruments trop nombreux, hypothèses de moments |
| Variables discrètes | Logit conditionnel, modèles mixtes, Poisson FE/PPML, autres comptages | Groupes sans variation, séparation, paramètres incidents, surdispersion selon le modèle |
| Panels macro | CCE, mean group, pooled mean group, racines unitaires et cointégration | Dépendance transversale, homogénéité des pentes, dimensions N/T, ordre d'intégration |
| Évaluation causale | DiD adaptée à l'adoption échelonnée, études d'événement | Définition de l'effet cible, tendances parallèles, anticipation, hétérogénéité |
| Sélection/attrition | Modèles ou pondérations adaptés au plan de collecte | Mécanisme d'observation et hypothèses d'identification, pas seulement équilibre du panel |

Ces lignes sont une feuille de route, pas des fonctions déjà livrées. Chaque ajout
nécessitera une spécification de l'estimand, des hypothèses, des cas exclus, des
messages pédagogiques et une confrontation à un moteur de référence. Aucun ajout
ne doit transformer un diagnostic en règle mécanique de sélection.

## Références de mise en œuvre

- [Documentation et catalogue plm](https://cran.r-project.org/package=plm)
- [Estimation des modèles plm](https://rdrr.io/cran/plm/man/plm.html)
- [Covariances robustes plm](https://rdrr.io/cran/plm/man/vcovHC.plm.html)
- [Hausman : méthode classique et auxiliaire robuste](https://rdrr.io/cran/plm/man/phtest.html)
- [Test de Wooldridge pour autocorrélation FE](https://rdrr.io/cran/plm/man/pwartest.html)

Les références concernent les méthodes et moteurs disponibles. Elles ne constituent
pas une validation de l'adaptateur econcompare : ses tests R restent à exécuter.

La 0.14.0 initialise le logit conditionnel exact, Poisson FE et FE-IV individuels. Les variantes mixtes, dynamiques, multinomiales, offsets et inférences avancées restent hors périmètre. Voir VALIDATION_0.14.0.md pour les limites et le statut de validation.
