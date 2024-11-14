# Contributing to ForestForesight

This workflow guides contributors on how to address **[confirmed]** issues in alignment with our branching, testing, and styling strategies.

## Table of Contents
1. [Valid Issues](#valid-issues)
2. [Workflow](#workflow)
   - [1. Assign the Issue](#1-assign-the-issue)
   - [2. Fork the Repository](#2-fork-the-repository)
   - [3. Develop and Test Your Changes](#3-develop-and-test-your-changes)
   - [4. Create a Pull Request (PR)](#4-create-a-pull-request-pr)
   - [5. Review and Respond](#5-review-and-respond)
   - [6. Approval and Merge](#6-approval-and-merge)

---

## Valid Issues
Only issues with the label **[confirmed]** should be picked up and fixed. If you are interested in a new feature or bug fix, please check if there is already an issue created. If not, create a new issue.
**Note:** Only issues labeled **[confirmed]** are eligible to be merged into the repository.

## Workflow

### 1. Assign the Issue
   - Find an open **[confirmed]** issue that you would like to work on. If there is no assignee, check the comments to ensure no one else is already working on it. Then, write a comment indicating your interest in working on the issue. You can begin working on it, and maintainers may later review your request and assign the issue to you.

### 2. Fork the Repository
   - If you have not already, fork the repository to create a copy under your GitHub account. This will allow you to make changes without direct access to the main repository. You can start working on the `develop` branch or create a specific branch for the issue in your forked repository.

### 3. Develop and Test Your Changes
   - After making the necessary changes in your branch, follow our [Testing Strategy](https://forestforesight.atlassian.net/wiki/spaces/EWS/overview?homepageId=32961) to ensure your changes are covered by unit tests.
   - Ensure code quality by running the `styler` and `lintr` checks. You can follow the instructions in the [Tools](https://forestforesight.atlassian.net/wiki/spaces/EWS/overview?homepageId=32961) section to use `styler` and `lintr`.
   - Confirm that all code meets the project’s standards before submitting.

### 4. Create a Pull Request (PR)
   - Once your changes are complete and tested, open a PR from your forked repository’s branch to the main repository’s `develop` branch.
   - Ensure your PR:
     - Clearly references the issue it addresses by using `#issue-number` (e.g., `#123`).
     - Follows our testing and styling guidelines.
     - Is ready for review by maintainers.
  - **Note:** Pull Requests without clear information will take longer and may even be rejected.

### 5. Review and Respond
   - Wait for maintainers to review your PR. They will either approve the PR if it meets all criteria or request changes if adjustments are needed.
   - If changes are requested:
     - Respond to comments and make necessary updates at your earliest convenience.
     - Push the updates to your branch in the forked repository; these changes will be reflected in the PR. And wait for the review.

### 6. Approval and Merge
   - Once your PR is approved, a maintainer will merge it into the `develop` branch in the main repository.
   - After merging, the issue will be closed, and your contribution will become part of the project.

Thank you for contributing and helping us improve this project! Please ensure all contributions align with our guidelines to maintain quality and consistency across the codebase. 
**Note:** Our guidelines are continually being improved, so please check back regularly for any updates.
