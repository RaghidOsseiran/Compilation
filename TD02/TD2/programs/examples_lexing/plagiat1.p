/*Ceci est une belle fonction que j’ai codée moi-même.*/
int ma_fonction (int[] tab, var float control) ::=
{   //Là c’est le début.
    variable := control - 20.0;
    int i;
    i := 0; //Ici on met 0 dans i
    while(i < tab.size)
    {
        if (control * variable <= 3.0)
            then tab[i] := tab[i] - 1;
            else tab[i] := tab[i] + 1;
        i := i + 1;
        //et là on termine la boucle.
    }
}
