---
title: "Analyse Économétrique : Déterminants de la Baisse des Naissances en France (1991-2019)"
---

##  Présentation du Projet

Ce projet vise à analyser les facteurs influençant la baisse du taux de fécondité en France depuis 2010 à l'aide d'une approche économétrique. Nous avons traité des séries chronologiques sur la période **1991-2019** en utilisant **R** pour la modélisation statistique.

##  Objectifs

- Identifier les déterminants socio-économiques, démographiques et culturels de la baisse des naissances.
- Construire un modèle économétrique robuste en corrigeant les biais d'hétéroscédasticité et d'autocorrélation.
- Tester l'endogénéité des variables explicatives à l'aide de la méthode **2SLS** (variables instrumentales).

##  Méthodologie

### Collecte et Préparation des Données :

- Sélection de 37 variables explicatives (économiques, démographiques, comportementales).
- Nettoyage, interpolation trimestrielle et gestion des valeurs manquantes.

### Modélisation Économétrique

- Régression OLS avec transformation logarithmique.
- Traitement des problèmes d'endogénéité (tests de Hausman, Sargan).
- Correction des biais d'hétéroscédasticité (tests de White, Breusch-Pagan) et d'autocorrélation (modèle ARIMA).

### Résultats :

- Identification des variables les plus significatives sur la dynamique du taux de fécondité.
- Validation de la robustesse du modèle final avec des résidus normalisés.

##  Principaux Enseignements

- Des facteurs comme la précarité féminine, le temps partiel, et les préoccupations environnementales influencent significativement la fécondité.
- La prise en compte de la structure temporelle et des effets retardés améliore la précision des estimations.

##  Auteurs

- Andrea Chahwan
- Lauriane Delpont
- Mehdi Fehri
- Niclette Ndaya Nsabua
