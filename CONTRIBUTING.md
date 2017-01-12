# Contributing

When contributing to this repository, please first discuss the change you wish to make via email or other electronic correspondence with the project maintainers before making a change.  

Please note we have a code of conduct, please follow it in all your interactions with the project.

# Suggesting Enhancements
Enhancement suggestions are tracked as GitHub issues. Following these guidelines to submit your enhancement request.  

## Before Submitting An Enhancement Suggestion
* Check the project issues to make sure that your suggestion is not a duplicate of an existing issue.   
* Make sure that you have read and understood the documentation for the the corresponding [BLAS library](http://netlib.org/blas) functions.
* Make sure that you have read the project issues board and that you have identified any related issues.
* Provide the project owner(s) with a clear description for the enhancement.

## Submitting an Enhancement Suggestion
After following the steps above, you may create a new GitHub issue for your enhancement suggestion.   Please use the following best practices:

* Create a clear title of the enhancement using user story format.   User stories are of the form

    ``` 
    As a <type of user>, I want <goal> so that <benefit>
    ```

* Create a comment on the issue that gives a clear, detailed description of the work you intend to undertake.   Citing references is highly recommended to help other contributors understand your proposal.    Describe the tests that must be implemented to evidence the correctness of the implementation.
* Label the issue with "Enhancement"
* Don't forget to assign the issue to a contributor.

## Pull Request Process
Pull requests are the formal gatekeeping process to ensure consistent implementation of coding standards have been implemented in each change-set prior to introduction into the master branch.   Pushing local changes to the origin/master branch will be rejected.   

* Create a descriptive title for the pull request.   Mention the issue numbers addressed by the pull request in the pull request title.   Pull requests without associated issues will be summarily discarded.
* Create a description that clearly describes all the changes that were made as part of the change-set, and how they relate to the issue(s) being addressed.   All changes should be necessary for one or more of the issues being addressed.
* Ensure that you have unit-tests that strongly evidence the correctness of your code.  Be careful to consider corner cases in floating point data.
* Ensure that all your code is sufficiently commented.   All top level functions should include a haddock comment describing their use, and (if applicable) correctness conditions.   It is strongly encouraged that contributors also comment details of any algorithms, and include links to references where appropriate.   
* Your pull request must be reviewed by at least one other contributor.
* You may merge the Pull Request in once you have the sign-off of the project owner, and you have attached to the issue evidence and that the software builds and tests pass.  

## How Do I Submit A (Good) Bug Report?
Bugs are tracked as GitHub issues.   A good bug report should explain the problem and include additional details to help your fellow contributors reproduce the problem.

* Use a clear and descriptive title for the issue to identify the problem.
* Describe the exact steps which reproduce the problem in as many details as possible.
* Attach examples to demonstrate the bug, for example: links to files or GitHub projects, screen shots or code snippets.
* Describe the behavior you observed after following the steps and point out what exactly is the problem with that behavior.
* Explain which behavior you expected to see instead and why.
* If the problem is related to performance, include a CPU profile capture and a screenshot with your report.

# Code of Conduct

In the interest of ensuring an open and welcoming environment, all
contributors must must pledge to make participation in the project a harassment-free experience for everyone:

* Use welcoming and inclusive language
* Foster exchange of ideas and differing viewpoints
* Gracefully accept constructive criticism
* Focus on what is best for the project

Project maintainers have the right and responsibility to remove, edit, or reject any contributions that are not aligned to the Code of Conduct, or ban any contributor for other behaviors are deem inappropriate, threatening, offensive, or harmful.
