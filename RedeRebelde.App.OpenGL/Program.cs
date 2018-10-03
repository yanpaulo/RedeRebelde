using System;

namespace RedeRebelde.App.OpenGL
{
    /// <summary>
    /// The main class.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
#if !DISABLE_XOR
            Application.ResultadoXOR = AlgoritmoClassificacao.xor();
            Console.WriteLine(Application.ResultadoXOR);
#endif
            Application.ResultadoRegressao = AlgoritmoRegressao.resultado();
            Console.WriteLine(Application.ResultadoRegressao);

            using (var game = new Game1())
                game.Run();
        }
    }
}
