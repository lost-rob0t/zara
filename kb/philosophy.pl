:- module(kb_philosophy,
    [
        philosopher/4,
        school/2,
        concept/2,
        position/3,
        argument/5,
        objection/3,
        fallacy/2,
        alias/2
    ]).

school(virtue_ethics, "Ethics centered on character, cultivated dispositions, and human flourishing.").
school(stoicism, "Hellenistic philosophy emphasizing virtue, disciplined judgment, and acceptance of what is not under one's control.").
school(deontology, "Ethics centered on duties, rules, and the form of morally permissible action.").
school(utilitarianism, "Consequentialist ethics evaluating actions by their effects on aggregate well-being.").
school(existentialism, "Philosophy emphasizing freedom, responsibility, situated existence, and the creation or discovery of meaning.").
school(absurdism, "Philosophy examining the conflict between the human demand for meaning and an indifferent world.").
school(empiricism, "Epistemology grounding knowledge primarily in experience and observation.").
school(rationalism, "Epistemology emphasizing reason and structures of thought as major sources of knowledge.").
school(pragmatism, "Philosophy evaluating ideas through inquiry, consequences, practices, and lived problem solving.").
school(confucianism, "Ethical and political tradition emphasizing cultivated character, relationships, ritual, and humane conduct.").
school(daoism, "Tradition emphasizing naturalness, non-forcing, perspective, and alignment with the Dao.").
school(madhyamaka, "Buddhist philosophical tradition analyzing dependent arising and the emptiness of intrinsic existence.").

philosopher(socrates, "Socrates", socratic, ancient).
philosopher(plato, "Plato", platonism, ancient).
philosopher(aristotle, "Aristotle", virtue_ethics, ancient).
philosopher(epictetus, "Epictetus", stoicism, ancient).
philosopher(marcus_aurelius, "Marcus Aurelius", stoicism, ancient).
philosopher(descartes, "René Descartes", rationalism, modern).
philosopher(hume, "David Hume", empiricism, modern).
philosopher(kant, "Immanuel Kant", deontology, modern).
philosopher(mill, "John Stuart Mill", utilitarianism, modern).
philosopher(kierkegaard, "Søren Kierkegaard", existentialism, modern).
philosopher(nietzsche, "Friedrich Nietzsche", genealogy, modern).
philosopher(sartre, "Jean-Paul Sartre", existentialism, contemporary).
philosopher(beauvoir, "Simone de Beauvoir", existentialism, contemporary).
philosopher(camus, "Albert Camus", absurdism, contemporary).
philosopher(confucius, "Confucius", confucianism, ancient).
philosopher(zhuangzi, "Zhuangzi", daoism, ancient).
philosopher(nagarjuna, "Nāgārjuna", madhyamaka, ancient).
philosopher(dewey, "John Dewey", pragmatism, contemporary).

concept(virtue_ethics, "Moral evaluation centers on cultivated character, practical wisdom, and the habits needed for human flourishing.").
concept(stoicism, "Focus attention on judgments and actions under your control while meeting external events without making them the source of moral worth.").
concept(deontology, "Moral constraints and duties can bind independently of whether breaking them would improve aggregate outcomes.").
concept(utilitarianism, "Actions are evaluated by their consequences for welfare, classically by promoting the greatest overall happiness.").
concept(existentialism, "Human beings confront freedom, responsibility, finitude, and the task of living without outsourcing authorship of their choices.").
concept(absurdism, "The absurd names the tension between the demand for ultimate meaning and a world that does not supply it on demand.").
concept(empiricism, "Experience and observation are primary constraints on justified claims about the world.").
concept(rationalism, "Reason can provide substantive knowledge or structure that is not reducible to accumulated sensory experience.").
concept(pragmatism, "Ideas are tested through inquiry and the practical differences they make in experience rather than by detached correspondence alone.").
concept(nihilism, "A family of views denying some alleged source of objective meaning, value, truth, or authority; the exact claim depends on the domain.").
concept(eternal_recurrence, "Nietzsche's thought experiment asks whether one could affirm living the same life again in every detail.").
concept(categorical_imperative, "Kant's family of tests asks whether a maxim can be willed as universal law and whether persons are treated as ends rather than merely as means.").
concept(veil_of_ignorance, "A device for reasoning about fair principles while bracketing knowledge of one's own social position and natural advantages.").
concept(dependent_arising, "Phenomena arise dependently through conditions and relations rather than from self-sufficient intrinsic essences.").

position(kant, morality, "Moral worth is grounded in acting from duty according to principles that rational agents can will universally, while treating persons as ends.").
position(mill, morality, "Right action is ultimately assessed by its consequences for happiness and the prevention of suffering.").
position(aristotle, morality, "Ethics asks what kind of person to become and how practical wisdom shapes virtuous action toward flourishing.").
position(nietzsche, morality, "Inherited moral systems should be examined genealogically for the values, forces, and forms of life they express.").
position(epictetus, control, "Our judgments, choices, and aims are ours to govern; bodies, reputation, and external events are not fully under our control.").
position(sartre, freedom, "Human beings are radically responsible for what they make of their situation even though freedom is always exercised within concrete facticity.").
position(beauvoir, freedom, "Freedom becomes ethically meaningful through projects that acknowledge both one's own ambiguity and the freedom of others.").
position(camus, meaning, "The absence of guaranteed ultimate meaning is answered by lucid revolt and continued engagement rather than philosophical suicide.").
position(kierkegaard, meaning, "Existence cannot be reduced to detached system building; commitment and inward appropriation matter to how a person lives a truth.").
position(hume, knowledge, "Reasoning about matters of fact depends on experience and habits of expectation rather than demonstrative proof of necessary causal connections.").
position(descartes, knowledge, "Methodic doubt seeks a secure rational foundation from which knowledge can be rebuilt.").
position(dewey, knowledge, "Knowing is an activity within inquiry: beliefs are tools revised as organisms solve problems in environments.").
position(confucius, character, "Humane conduct is cultivated through learning, ritual practice, reflection, and responsible participation in relationships.").
position(zhuangzi, perspective, "Rigid distinctions can dissolve when viewed from wider perspectives; skilled action often works through responsiveness rather than forcing.").
position(nagarjuna, ontology, "Things lack independent intrinsic nature because they arise dependently; emptiness is not a separate substance behind appearances.").

argument(categorical_imperative, deontology, kant,
    [
        "A moral principle must be fit for rational agents as such rather than merely encode one person's convenience.",
        "A maxim that defeats itself when universalized cannot coherently function as a universal moral law.",
        "Rational persons must not be reduced to disposable instruments."
    ],
    "Test maxims for universalizability and treat persons as ends in themselves.").
argument(utilitarian_case, utilitarianism, mill,
    [
        "Pleasure and freedom from suffering matter to sentient beings.",
        "Each person's comparable welfare deserves impartial consideration.",
        "Consequences therefore supply a public basis for comparing candidate actions."
    ],
    "Prefer the available action whose consequences best promote overall well-being, subject to the theory's account of quality and rules.").
argument(stoic_control, stoicism, epictetus,
    [
        "Some things depend primarily on our judgments and choices.",
        "Many external events remain partly or wholly outside our control.",
        "Making tranquility depend on uncontrollable externals exposes agency to avoidable disturbance."
    ],
    "Direct effort toward judgment and action while accepting the limits of control over externals.").
argument(absurd_revolt, absurdism, camus,
    [
        "Humans seek intelligibility and meaning.",
        "The world does not guarantee a final answer to that demand.",
        "Inventing certainty or abandoning life evades rather than resolves the tension."
    ],
    "Live lucidly within the tension through revolt, freedom, and engagement.").

objection(categorical_imperative, demanding_conflicts, "Strict duties can appear to conflict or produce counterintuitive results in emergencies, motivating debates over formulation and application.").
objection(utilitarian_case, demandingness, "Maximizing aggregate welfare can appear excessively demanding and can threaten rights if protections are treated as merely contingent.").
objection(stoic_control, social_quietism, "A crude reading can slide from acceptance of limits into passivity, although Stoic sources also emphasize duties and action.").
objection(absurd_revolt, normative_gap, "Critics can ask why lucid recognition of the absurd specifically warrants revolt rather than another practical stance.").

fallacy(ad_hominem, "Attacking a person instead of addressing the relevant claim or argument.").
fallacy(straw_man, "Replacing an opponent's actual position with a weaker distortion and refuting the distortion.").
fallacy(false_dilemma, "Presenting too few alternatives as exhaustive when other live options exist.").
fallacy(begging_the_question, "Assuming the disputed conclusion in the premises or support offered for it.").
fallacy(equivocation, "Shifting the meaning of a key term across an argument while treating it as unchanged.").
fallacy(appeal_to_authority, "Treating an authority's assertion as sufficient when the authority is irrelevant, unreliable, or cannot substitute for the needed evidence.").

alias("virtue ethics", virtue_ethics).
alias("categorical imperative", categorical_imperative).
alias("eternal recurrence", eternal_recurrence).
alias("dependent arising", dependent_arising).
alias("immanuel kant", kant).
alias("john stuart mill", mill).
alias("friedrich nietzsche", nietzsche).
alias("jean-paul sartre", sartre).
alias("simone de beauvoir", beauvoir).
alias("albert camus", camus).
alias("marcus aurelius", marcus_aurelius).
alias("rené descartes", descartes).
alias("rene descartes", descartes).
alias("david hume", hume).
alias("john dewey", dewey).
alias("soren kierkegaard", kierkegaard).
alias("søren kierkegaard", kierkegaard).
alias("false dilemma", false_dilemma).
alias("straw man", straw_man).
alias("begging the question", begging_the_question).
alias("appeal to authority", appeal_to_authority).
