int main()
{
    int a = 0;
    for (int i = 0; i < 10; i++)
    {
        a += 2;
        if (a > 5)
            a--;
        else
            for (int j = 0; j < 5; j++)
                a += 3;
    }
    return a;
}

int func()
{
    int x = 10;
    for (int i = 12; i < x - 10; i++)
    {
        x += 2;
    }
}