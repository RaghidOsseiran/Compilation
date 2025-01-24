float toto(float bim, var bool[] tab) ::=
{
    pouf := 1.2 + bim - 7.5 * (4.3 / (52.3 % 12.2));
    tab[1] := pouf < 4.5;
    tab[2] := true;
    tab[3] := false;
    while(tab[2]) pouf := pouf + 2.3;
}