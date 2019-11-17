# Introductory Bayesian statistics for ecologists

18 November, 9:30 until 26 November 2019, 17:30<br/>
IGB, Müggelseedamm 310, lecture hall

## Course files
Please see the [files](files.html) page to access the data files you will need for the course, as well as lectures and other materials. Alternatively, you can get these at the [github repository](https://github.com/mtalluto/BayesCourseIGB).

## Course objectives

This course will cover the basics of Bayesian statistical methods with applications in ecology. Bayesian methods are a powerful set of tools that are increasingly used with complex ecological data. These methods can also be extended quite easily byond simple statistics to include process-based/mechanistic models. By the end of the course, participants should be able to:

* Understand basic concepts from probability theory
* Apply maximum likelihood and Bayes' Theorem to common statistical problems
* Express symbolically and in code the likelihood and prior probability of a Bayesian model
* Write a simple Markov-chain Monte Carlo (MCMC) sampler in R
* Write code for various models (including descriptive statistics, uni/multivariate linear regression, generalised linear models, and hierarchical models) in R or Stan
* Visualise and evaluate MCMC output

## Prerequisites

Participants will need to be comfortable programming in R and have a good understanding of basic statistics.

Students will work in small groups on a project building a Bayesian model from scratch. Any students that have data they wish to work on (and are willing to share with a group) are strongly encouraged to do bring their data to the class and to use them for the project. Students without data can find a project by working with a group or use data from freely available sources.

## What to bring
* A laptop with a recent version of R or Rstudio installed. Version 3.4 or greater is required. Please check before the course and update R if needed. Note that you might have to re-install packages after updating
* Please also install the following R packages: 
	* `rstan, bayesplot, devtools, ggplot2, data.table, mcmc`
* Data
	* If you have data you would like to analyse, please bring it! A major portion of the course will be designing a project in small groups. I imagine not everyone will have data, so we will do our best to match data-haves with the have-nots on the first day. If you have any questions about the data, feel free to ask me.


## General Schedule
Most days will consist of a mixture of lecture, individual coding practise, and work on group projects/presentations.

| Day          | Topics | Coding | Group Work |
|:-------------|:-------|:-------|:-----------|
| Monday 18    | Intro<br/> Probability theory<br/> Bayes' theorem<br/>Likelihood & optimisation | Probability exercises <br/> Single parameter estimation | Choose groups and projects |
| Tuesday 19   | Maximum Likelihood<br/>Applied Bayesian Methods<br/> MCMC<br/> | Writing a likelihood function<br/> | Develop project ideas and outline<br/>Choose model structure<br/>Discuss likelihood/priors
| Wednesday 20 | Metropolis-Hastings<br/> Intro to Stan<br/> Multivariate Bayesian models<br/> Prior choice<br/>Metropolis within Gibbs<br/> GLMs| Univariate Metropolis sampler<br/> Metropolis within Gibbs<br/>Simple Stan models| Develop likelihood and priors for your project
| Thursday 21  | | | Coding your project (on your own)
| Friday 22    | GLMs <br/> Laplace Approximation <br/> Hierarchical models | Advanced models | Working on projects
| Monday 25    | Model evaluation <br/> Diagnostics <br/> Model comparison  | Visualisation using `bayesplot` <br/>wAIC in Stan | Work on projects/presentations |
| Tuesday 26    | Additional topics (on-demand) | | Presentations |
