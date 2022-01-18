# Current biology edits to make

## Reviewer 1

- Hague et al. 2018, Evolution Letters found structural modifications in the garter snake Nav1.4 channel make the channel less excitable. Link: https://onlinelibrary.wiley.com/doi/full/10.1002/evl3.76
- Figure S1: specify that it is a species tree and not a gene tree.

## Reviewer 2

- [ ] Make a github repo, upload data and list that accession number in figures, where possible (
- [ ] Show the entire alignment Nav1.4 alignment in the supplement.
- [ ] Provide primers used for sequencing.
- [ ] Some questions to answer: Did you sequence in both directions? Were primers designed in regions flanking the start and stop coding or overlapping with it? In supplement I'd like to see this as a diagram. How do we know there has been no gene duplications? Did you detect any splice variants with your cDNA sequencing results? Are any known from this protein? Which is expressed in skeletal muscle if so? Omega is not really a proxy for selection. It is a proxy for Dn/Ds. These are not the same thing. If you mention "under positive selection" you should indicate the test used.
- [ ] Change all significant p values to < 0.01.
- [ ] Figure 6 caption: what is "planned comparisons"?
- [ ] why not also test double mutants? I don't see any reason to believe that the interaction between double mutants on physiology would be the same as triple or single mutants.
- [ ] Address this comment: I would like to see a more nuanced discussion of the dN/dS rate ratio parameter omega (which appears as the greek symbol and the letter "w" in the main and supplement). The statement that omega is a proxy for selection is bound to confuse readers--it is a parameter estimated under the specific model of codon evolution specified. I'd like to see a lot more detail on the specific parameters used in the branch and branch site tests as well as the bayes empircial bayes probabilities for sites. I liked that the authors use this as a way forward in terms of choosing which sites to focus on for their functional work, but the descriptions of what these tests do and the lack of information in the main text on how they were conducted detracts from the manuscript.

## Reviewer 3:

- [ ] Think about whether we need the PAML stuff. The PAML (pg 10) work feels tacked on. Is this really useful or necessary? It is not mentioned in the discussion. The authors do the hard work of assaying the functional significance of specific regions of Nav1.4—so why bother with models of selection?
- [ ] Correct this part of the mammalian phylogeny: Afrotheria & Xenarthra are at the base of the tree. Scandentia remains problematic. Or maybe take out the PAML part.
- [ ] Why do we add BSA to the venom tubes? Makes the venom less sticky to the tubes?! I think the tubes were pre-treated with BSA as well!
- [ ] Provide version numbers of all software used! Including R studio.
- [ ] Not sure what this comment means! Fig S1: fix Afrotheria & Xenarthra placement. Why aren't the most important taxa, the grasshopper mice, spelled out at the bottom of the tree? Citations for tree should be given in caption.


## Code based changes to figures:
- [ ] Think about a better naming scheme for Di, Diii, NC mutations
- [ ] Figure 2 panel b: change key to include diamonds and circles
- [ ] Remove redundant figure between 5 and 6
- [ ] For all bar charts show the underlying data points on the figure
- [ ] Figure 2: make it friendlier to color blind folks
- [ ] Figure 2: Change scientific name to common name for grasshopper mouse
- [ ] In Fig. 4a: do you need a y axis? Above Fig 4c, the "F" is cut off a bit and the x axis is screwed up in terms of spacing/alignment of labels.
- [ ] Figure out when the data are normally distributed whenever a t-test was used.
