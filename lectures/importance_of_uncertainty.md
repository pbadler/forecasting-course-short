---
title: "The Importance of Uncertainty"
output: html_document
layout: post
mathjax: true
---

Why is it so important for a forecast to include some information about uncertainty? Consider the following examples:

* For predictions of binomial events (yes/no), like whether or Trump will win the next US election, the probability of victory communicates the uncertainty. A 51% and 99% chance both predict victory for Trump, but differ enormously in their certainty. Similarly, if you are deciding whether or not to go camping, you will be interested in the difference between a 5% and 40% chance of rain.

* For a continuous response variable, the uncertainty is reported separately. For example, an economist might predict that Gross Domestic Product will grow at a rate of 2.0% in the next quarter. But that forecast should mean very different things to investors if the 95% confidence interval around that point forecast is 1.5 to 3.5% compared to -2.0 to 5.0%. 

* Without knowing the uncertainty, it is virtually impossible for a decision-maker to use a forecast.

### Sources of uncertainty

Different sources contribute to forecast uncertainty. Mike Dietze lists the following five important sources in Chapter 2 of his book:

* Observation error (can you know exactly how many elk are in the population at any point in time?)
* Initial conditions uncertainty (how fast is the economy growing right now?)
* Process variability (can your covariates explain all the variation in the response?)
* Parameter uncertainty (how precise are your parameter estimates?)
* Driver uncertainty (how rapidly will temperatures increase under climate change?)

This list could be expanded to include model selection uncertainty (perhaps a different model would make a different prediction?) or scenario uncertainty (how fast will greenhouse gas emissions rise or fall?).

Some of these source of uncertainty stay constant over time while others grow. Observation error stays constant--observation errors this year do not affect the magnitude of observation errors next year. In a dynamic model, such as a model of population growth, process variability often compounds over time. Initial conditions uncertainty will grow quickly if a dynamic system is chaotic, but will decrease over time if the system goes to equilibrium.

Identifying the relative contributions from these sources can be extremely valuable for determining how to improve a forecast. Weather prediction improved after researchers determined that initial conditinos were a large source of uncertainty, and then convinced governments to invest more resources in data collection to reduce that uncertainty.

Dietze emphasizes that the key distinction between *uncertainty* and *variability* is that uncertainty reflects our ignorance about a process and should, in principle, decrease with more observations (greater sample size), whereas variability concerns variation in the process itself that is not captured by the model and should not be expected to decrease as sample size increases.  

