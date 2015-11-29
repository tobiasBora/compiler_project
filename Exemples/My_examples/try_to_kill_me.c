#ifdef MCC
#else
#include <stdio.h>
#include <stdlib.h>
#endif


int* chaine_ok;
int* chaine_fail;
int* glob_int_1;


int test_global_1()
{
     glob_int_1++;
     return(2*glob_int_1);
}

int lots_args(int a, int b, int c, int d, int e, int f, int g, int h, int i)
{
     printf("Via appel interposé : %d %d %d %d %d %d %d %d %d\n", a,b,c,d,e,f,g,h,i);
     printf("Tous les arguments sont bons : %s\n", (a==1 && b==2 && c==3 && d==4 && e==5 && f==6 && g==7 && h==8 && i==9) ? chaine_ok : chaine_fail);
     return(42);
}


int fact(int n)
{
     if(n==0)
     {
	  return(1);
     }
     else
     {
	  return(n*fact(n-1));
     }
}

int jeux_plus_moins()
{
     int nombreMystere, nombreEntre, MAX, MIN;
     nombreMystere = 0;
     nombreEntre = malloc(8);
     nombreEntre[0] = -1;
     MAX = 100;
     MIN = 1;
     // Génération du nombre aléatoire
     srand(time(NULL));
     nombreMystere = (rand() % (MAX - MIN + 1)) + MIN;
     /* La boucle du programme. Elle se répète tant que l'utilisateur n'a pas trouvé le nombre mystère */
     while (nombreEntre[0] != nombreMystere)
     {
	  // On demande le nombre
	  printf("Quel est le nombre mystère (réponse : %d) ? ", nombreMystere);
	  scanf("%llu", nombreEntre);
	  // On compare le nombre entré avec le nombre mystère
	  if (nombreMystere > nombreEntre[0])
	       printf("C'est plus !\n\n");
	  else if (nombreMystere < nombreEntre[0])
	       printf("C'est moins !\n\n");
	  else
	       printf ("Bravo, vous avez trouve le nombre mystere !!!\n\n");
     }
}

int main (int argc, char **argv)
{
     int i,j,k,n;
     int* tableau;
     chaine_ok = "\033[1;32mOk\033[0;m";
     chaine_fail = "\033[1;31mFailed\033[0;m";
     printf("#################################################\n");
     printf("#################################################\n");
     printf("#################################################\n");
     printf("#################################################\n");
     printf("#################################################\n\n");
     printf("#################################################\n");
     printf("######### Test des affectations simples #########\n");
     printf("#################################################\n\n");
     printf("\n===== Logique =====\n");
     printf("Condition ternaire : %s\n", (1)? chaine_ok : chaine_fail);
     printf("Condition ternaire 2 : %s\n", (0)? chaine_fail : chaine_ok);
     printf("Non logique : %s\n", (!(0))? chaine_ok : chaine_fail);
     printf("Et logique : %s\n", (1 && 1) ? chaine_ok : chaine_fail);
     printf("Et logique 2 : %s\n", ((1 && 1) && (!(1 && 0)) && (!(0 && 1)) && (!(0&&0))) ? chaine_ok : chaine_fail);
     printf("Ou logique : %s\n", ((1 || 1) && (1 || 0) && (1 || 0) && (!(0 ||0))) ? chaine_ok : chaine_fail);
     printf("Egalite : %s\n", ((47==47) && (!(47 == 50)))? chaine_ok : chaine_fail);
     printf("Non egalite : %s\n", ((!(47!=47)) && (47 != 50))? chaine_ok : chaine_fail);
     printf("\n===== Affectations ===\n");
     i=0;
     j=1;
     printf("Passage de variable : %s\n", ((!i) && j)? chaine_ok : chaine_fail);
     k=46;
     k=477;
     printf("Affectation de k : %s\n", (k==477)? chaine_ok : chaine_fail);

     
     printf("\n=== Comparaisons =====\n");
     printf("Egalité : %s\n", (17==17 && (!(64==46)))? chaine_ok : chaine_fail);
     printf("Inférieur stricte : %s\n", ((1<5) && (!(5<5)))? chaine_ok : chaine_fail);
     printf("Inférieur large : %s\n", ((16<=30) && (10<=10) && (!(16<=15)))? chaine_ok : chaine_fail);
     printf("Supérieur stricte : %s\n", (((30>4)) && (!(5>5)))? chaine_ok : chaine_fail);
     printf("Supérieur large : %s\n", ((!(16>=30)) && (10>=10) && (16>=15))? chaine_ok : chaine_fail);
     k=50;
     i=60;
     printf("Supérieur stricte v2 : %s\n", (i>k) ? chaine_ok : chaine_fail);


     printf("\n===== Opérateurs unaire =====\n");
     k=477;
     printf("Moins 1 opérateur : %s\n", ((-k==-477) && (-k != 477)) ? chaine_ok : chaine_fail);
     printf("Negation logique : %s\n", (~k+k+1==0) ? chaine_ok : chaine_fail);
     printf("Test k non modifié : %s\n", (k==477) ? chaine_ok : chaine_fail);
     i = 46;
     printf("Test post_inc : %s\n", (i++==46) ? chaine_ok : chaine_fail);
     printf("Test post_inc 2 : %s\n", (i==47) ? chaine_ok : chaine_fail);
     i = 467;
     printf("Test pre_inc : %s\n", (++i==468) ? chaine_ok : chaine_fail);
     i = 246;
     printf("Test post_dec : %s\n", (i--==246) ? chaine_ok : chaine_fail);
     printf("Test post_dec 2 : %s\n", (i==245) ? chaine_ok : chaine_fail);
     i = 584;
     printf("Test pre_inc : %s\n", (--i==583) ? chaine_ok : chaine_fail);

     printf("\n===== Séquence =====\n");
     k=807;
     i=42;
     printf("Séquence : %s\n", ((i,k,45)==45) ? chaine_ok : chaine_fail);
     printf("Séquence variable : %s\n", ((45,i,k)==807) ? chaine_ok : chaine_fail);
     printf("Séquence effet de bord : %s\n", ((i++,i++,i++,i++)==45) ? chaine_ok : chaine_fail);

     printf("\n===== Opérateurs binaires =====\n");
     k=477;
     i = 13;
     j = k+i;
     printf("Addition à côté : %s\n", (j==490) ? chaine_ok : chaine_fail);
     printf("Addition : %s\n", (i+k==490) ? chaine_ok : chaine_fail);
     printf("Soustration : %s\n", (k-i==464) ? chaine_ok : chaine_fail);
     printf("Multiplication : %s\n", (i*k==6201) ? chaine_ok : chaine_fail);
     printf("Division : %s\n", (k/i==36) ? chaine_ok : chaine_fail);
     printf("Reste (modulo) : %s\n", (k%i==9) ? chaine_ok : chaine_fail);

     printf("\n===== Opérateurs binaires en regardant le sens d'évaluation (par forcément comme gcc) =====\n");
     i = 400;
     j = 56;
     printf("Addition droite à gauche : %s\n", (( i + (i++,i++,j) ) == 458) ? chaine_ok : chaine_fail);
     i = 400;
     j = 53;
     printf("Addition droite à gauche 2 : %s\n", (( (i++,i++,j) + i  ) == 453) ? chaine_ok : chaine_fail);
     i = 400;
     j = 53;
     printf("Addition droite à gauche 3 : %s\n", (( (i++,i++,i++) + i  ) == 802) ? chaine_ok : chaine_fail);
     i = 400;
     j = 56;
     printf("Soustration droite à gauche : %s\n", (( i - (i++,i++,j) ) == 346) ? chaine_ok : chaine_fail);
     i = 400;
     j = 53;
     printf("Soustraction droite à gauche 2 : %s\n", (( (i++,i++,j) - i  ) == -347) ? chaine_ok : chaine_fail);
     i = 400;
     j = 53;
     printf("Soustraction droite à gauche 3 : %s\n", (( (i++,i++,i++) - i  ) == 2) ? chaine_ok : chaine_fail);
     i = 400;
     j = 53;
     printf("Multiplication droite à gauche : %s\n", (( (i++,i++,j) * i  ) == 21200) ? chaine_ok : chaine_fail);
     i = 400;
     j = 53;
     printf("Division droite à gauche : %s\n", (( (j++,j++,i) / j  ) == 7) ? chaine_ok : chaine_fail);
     i = 400;
     j = 53;
     printf("Reste droite à gauche (modulo) : %s\n", (( (j++,j++,i) % j  ) == 29) ? chaine_ok : chaine_fail);

     printf("\n===== Variables globales =====\n");
     glob_int_1=761;
     printf("Test variables globales et retour fonction : %s\n", ((test_global_1() == 1524) && (glob_int_1 == 762)) ? chaine_ok : chaine_fail);
     
     

     printf("\n===== Essayer de faire planter appel fonction avec plein d'arguments  =====\n");
     printf("Simple appel à printf : %d %d %d %d %d %d %d %d %d\n", 1,2,3,4,5,6,7,8,9);
     k=lots_args(1,2,3,4,5,6,7,8,9);
     printf("Retour de la grosse fonction : %s \n", (k=42)? chaine_ok :chaine_fail);
     printf("Evaluation de droite à gauche : \n");
     i=9;
     lots_args(i--,i--,i--,i--,i--,i--,i--,i--,i--);

     
     printf("\n===== Essayer de faire planter la factorielle =====\n");
     for(i=0;i<=20;i++)
     {
     	  k=fact(i);
     	  printf("fact(%d)=%llu\n", i,k);
     }
     printf("Test factorielle (fact(20)=2432902008176640000) : %s\n", (fact(12)==479001600) ? chaine_ok : chaine_fail);

     printf("\n===== Les conditions =====\n");
     i = 91;
     j = 0;
     k = 0;
     if (i == 91)
	  j = 1;
     else
	  j = -1;
     if (i == 92)
	  k = 1;
     else
	  k = -1;
     printf("Condition basique : %s\n", ((j == 1) && (k == -1)) ? chaine_ok : chaine_fail);
     
     printf("\n===== Les boucles =====\n");
     j = 0;
     for(i=0;i<10;i++)
     {
	  j = 2*j + i;
	  printf("%d -> %d; ", i, j);
     }
     printf("\nBoucle 1 : %s\n", ((j == 1013) && (i == 10)) ? chaine_ok : chaine_fail);
     j = 0;
     k = -56;
     /* Calcule la somme des 6 premieres factorielles */
     for(i=0;i<6;i++)
     {
	  int r,k;
	  r = 1;
	  k = i;
	  /* Calcule la factorielle */
	  while(k>=1)
	  {
	       r = r * k;
	       k--;
	  }
	  j = j + r;
     }
     printf("Boucle 2 : %s\n", (j == 154) ? chaine_ok : chaine_fail);
     printf("Préserve variables : %s\n", (k == -56) ? chaine_ok : chaine_fail);
     
     printf("\n===== Jouer avec les tableaux et malloc =====\n");
     n=20;
     tableau = malloc(8*n);
     /* Init */
     for(i=0;i<n;i++)
     {
	  tableau[i] = i*i;
	  printf("tableau[%d] = %d (théorie : %d) : %s\n", i, tableau[i], i*i, (tableau[i] == i*i) ? chaine_ok : chaine_fail);
     }
     printf("tableau[0]++ étape 1 : %s\n", (tableau[0]++ == 0) ? chaine_ok : chaine_fail);
     printf("tableau[0]++ étape 2 : %s\n", (tableau[0] == 1) ? chaine_ok : chaine_fail);
     printf("tableau[n/2]++ étape 1 : %s\n", (tableau[n/2]++ == (n/2)*(n/2)) ? chaine_ok : chaine_fail);
     printf("tableau[n/2]++ étape 2 : %s\n", (tableau[n/2] == (n/2)*(n/2) + 1) ? chaine_ok : chaine_fail);
     printf("++tableau[n/2] étape 1 : %s\n", (++tableau[n/2] == (n/2)*(n/2) + 2) ? chaine_ok : chaine_fail);
     printf("++tableau[n/2] étape 2 : %s\n", (tableau[n/2] == (n/2)*(n/2) + 2) ? chaine_ok : chaine_fail);
     printf("tableau[n/2]-- étape 1 : %s\n", (tableau[n/2]-- == (n/2)*(n/2) + 2) ? chaine_ok : chaine_fail);
     printf("tableau[n/2]-- étape 2 : %s\n", (tableau[n/2] == (n/2)*(n/2) + 1) ? chaine_ok : chaine_fail);
     printf("--tableau[n/2] étape 1 : %s\n", (--tableau[n/2] == (n/2)*(n/2)) ? chaine_ok : chaine_fail);
     printf("--tableau[n/2] étape 2 : %s\n", (tableau[n/2] == (n/2)*(n/2)) ? chaine_ok : chaine_fail);
     
     printf("\n===== Scanf =====\n");
     printf("Ecrivez le nombre 60 : \n");
     scanf("%llu",tableau);
     printf("60(:=%d) est supérieur à 50 : %s\n",tableau[0], (tableau[0]>50) ? chaine_ok : chaine_fail);
     printf("Ecrivez le nombre 10 : \n");
     scanf("%llu",tableau);
     printf("10(:=%d) est inférieur à 50 : %s\n",tableau[0], (tableau[0]<50) ? chaine_ok : chaine_fail);

     printf("\n===== Jeux du plus ou moins =====\n");
     jeux_plus_moins();


     


/* while(fact(10,1,2,3,4,5,6,7,8),++i) */
     /* { */
     /* 	  printf("%llu\n", i); */
     /* } */
     return(0);
}
