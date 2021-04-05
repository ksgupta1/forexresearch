# forexresearch
Forex spreads and correlations with other assets


Description of the most interesting data analysis you've done, key findings, and its impact 

Objective: 

The objective of the data analysis was to understand the cross-asset volatility and develop a hedging strategy for the firm portfolio which consisted of forex, commodities, equities and fixed income.

Work Done: 

Developed a model to understand and predict the volatility of different assets. Ran time-series analysis such as auto-regressive conditional herescadesity and various moving average techniques like EWMA to build a predictive model. Wrote back-testing framework to experiment and test various ideas. This back-testing framework was also connected to trading production system to gauge the actual results from theoretical results and calculate execution impact costs (i.e. slippage)

Key Findings:

The major findings were –

1.	Even though returns across asset classes is uncorrelated to each other, the volatility is clustered across asset classes. That means an extreme movement in forex market overlaps with extreme movements in equity markets, bond markets as well.
2.	Therefore, a single liquid instrument can be used to hedge the risk across all the asset classes in addition to gaining return diversification across assets by investing across assets

Impact:
 
As final part of the project, I built a trading strategy to hedge the firm portfolio. This strategy generated $5 million-dollar (Over the course of 4 years) of Profit for the firm especially during volatile events such as Jan 2015 (Swiss Franc Peg Scrapped) August 2015 (Chinese Stock Market Sell off), June 2016 (Brexit Vote), Feb 2018 (Volmageddon VIX ETF Blowup). The strategy performed favorably during the Feb 2020 (Coronavirus Pandemic) as well.
