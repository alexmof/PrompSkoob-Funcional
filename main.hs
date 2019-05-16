exibeMenuUsuario :: String -> String
exibeMenuUsuario nome =  " .::. PrompSkoob .::.\n - Menu Principal - \n Ola, " ++ nome ++ "!\n\n (1) Editar meu perfil\n (2) Gerenciar livro\n (3) Minha estante\n (4) Pesquisar no acervo\n (5) Recomendações de livros\n (6) Remover meu perfil\n (7) Sair da conta\n (8) Sair\n\nOpcao: "

exibeMenuVisitante :: String -> String
exibeMenuVisitante nome = " .::. PrompSkoob .::.\n - Menu Principal - \n Ola, " ++ nome ++ "!\n (1) Quero me cadastrar\n (2) Autenticacao\n (3) Pesquisar no acervo\n (4) Sair\n\n Opcao: "

exibeMenu :: Bool -> String -> String
exibeMenu True nome = exibeMenuUsuario nome
exibeMenu False nome = exibeMenuVisitante nome

validaOpcao :: Int -> Bool -> String
validaOpcao opcao estaLogado 
    | estaLogado == True && opcao < 1 || opcao > 8 = "Opcao invalida!\n"
    | estaLogado == False && opcao < 1 || opcao > 4 = "Opcao invalida!\n"
    | otherwise = ""

acaoMenu :: Int -> Bool ->
acaoMenu 1 False = cadastro
acaoMenu 2 False = autenticacao
acaoMenu 3 False = pesquisarNoAcervo
acaoMenu 4 False = sair
acaoMenu 1 True = editarPerfil
acaoMenu 2 True = gerenciaLivro
acaoMenu 3 True = minhaEstante
acaoMenu 4 True = pesquisarNoAcervo
acaoMenu 5 True = recomendaLivros
acaoMenu 6 True = removePerfil
acaoMenu 7 True = sairConta
acaoMenu 8 True = sair

main = do
    let estaLogado = False
    let nome = "Mica"
    putStrLn (exibeMenu estaLogado nome)
    opcao <- readLn :: IO Int
    putStrLn (validaOpcao opcao estaLogado)
    acaoMenu opcao estaLogado