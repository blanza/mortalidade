# Mortalidade
Rotinas para replicar o artigo "Diferenciais de mortalidade por grupos de beneficiários do Instituto Nacional do Seguro Social". O repositório contém as rotinas elaboradas em R para gerar todas as estimativas e gráficos. Também disponibilizamos as tabelas em formato excel e cvs. As rotinas permitem a replicação de todas as análises feitas no artigo. 

# Resumo

Esse trabalho tem por objetivo estimar a mortalidade e analisar seus diferenciais por sexo, idade e grupos de beneficiárias do Instituto Nacional de Seguro Social Brasileiro em 2015; fazer comparações com estimativas oficiais para a população geral, avaliando a distribuição dos óbitos por idade e da sobrevida a partir dos 65 anos. Os dados de óbitos e população são dos registros administrativos do Instituto Nacional de Seguro Social Brasileiro. Utilizou-se os modelos Gompertz e Van der Maen, e regressão TOPALS, para estimar as taxas de mortalidade acima de 65 anos, segundo grupos de beneficiários: aposentados por idade do Regime Geral de Previdência Social - desagregados por Clientelas Urbana e Rural; aposentados por tempo de contribuição e beneficiários de amparos assistenciais para idosos de baixa renda. Entre os principais resultados, foi possível minimizar o crossover nas taxas de mortalidade das idades avançadas, quando a mortalidade da população menos favorecida se torna menor do que mortalidade de populações com melhores indicadores sociais. Cotejando os resultados com as estimativas oficiais de mortalidade, observou-se que as esperanças de vida às idades de 65 e 75 anos para a população alvo desse estudo são maiores do que na população geral. 

# Arquivos e pacote

O artigo pode ser replicado usando as rotinas disponíveis em cada uma das pastas do repositório (<nome do arquivo>.R). O R é um software livre que pode ser obtido em: https://cran.r-project.org/ .
  
Os mesmos arquivos foram disponibilizados em formato CSV e MS-Excel. Para o uso das rotinas em R, o usuário deve usar os arquivos <nome do arquivo>.csv . Os arquivos <nome do arquivo.xlsx> pode ser abertos nos programas MS-Excel ou Libre Office. As tabelas de vida geradas também estão dispoíveis em formato MS-Excel. 
 
  
# Descrição dos Arquivos

1. Avaliação da Qualidade das Informações
- As rotinas e dados permitem avaliar a qualidade dos dados de mortalidade em relação a preferência por digitos e em relação a sobre-declaração da idade a morte usando dois métodos diferentes. É necessário instalar no R o pacote Demotools (https://timriffe.github.io/DemoTools/)
  
2. Seleção das idades para usar as leis de mortalidade
- as rotinas avaliam e indicam qual as idades devem ser usadas na aplicação das Leis de Mortalidade. Usamos o RMSE para definir qual o intervalo de idade mais adequado para o desenvolvimento das próximas etapas do trabalho. 
  
3. Aplicação das Leis de Mortalidade
- 
  
4. Estimativas das tabelas de vida
5. Estimativas das medidas sintéticas
6. Rotinas para os gráficos. 
