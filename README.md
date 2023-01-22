# Sous-groupes de ℤ/nℤ × ℤ/nℤ

*Projet proposé par Olivier Brunat*

Il est très facile de décrire tous les sous-groupes d’un groupe cyclique 
d’ordre *n* : il y en a exactement un par diviseur positif de *n*. Par ailleurs, 
on peut montrer que tout groupe abélien fini est un produit direct de groupes 
cycliques. Pourtant, étonnamment, décrire tous les sous-groupes d’un tel groupe 
est en général un problème difficile. 

Dans ce projet, on se propose de considérer cette question pour le groupe 
ℤ/nℤ × ℤ/nℤ. Les prérequis pour ce projet est d’avoir suivi (et aimer !) 
Algèbre 1.

## D’un point de vue théorique

0n aura besoin de comprendre des résultats sur les réseaux de ℤᵐ. 
On s’intéressera en particulier aux matrices à coefficients entiers et 
aux sous-groupes de ℤᵐ engendrés par les vecteurs colonne d’une telle matrice. 
On étudiera les *formes normales de Hermite et de Smith* d’une matrice à 
coefficients entiers. Ces techniques sur les matrices entières sont d’un 
intérêt qui dépassent la problématique proposée dans ce projet.
Par exemple, des méthodes en cryptographie actuelle sont basées sur ces outils.

Une autre attaque du problème, davantage axée sur la théorie des groupes finis, 
pourra aussi être étudiée à travers le lemme de Goursat. 

On comparera les deux approches.


## D’un point de vue pratique

On implémentera des algorithmes permettant de produire les
formes de Hermite et de Smith d’une matrice à coefficients entiers. 
On produira également un programme donnant le treillis des sous-groupes 
d’un groupe de la forme ℤ/nℤ × ℤ/nℤ où *n* est un entier naturel 
donné explicitement en entrée.

## Rendu

Ecrire un rapport, reprenant la démarche mathématique pour la résolution 
du problème ainsi que les principaux alorithmes utilisés. On pourra y ajouter 
les problèmes rencontrés, la comparaison des deux pistes avec si possible
une analyse de performance, d'adaptabilité, etc.

