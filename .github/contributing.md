# Contributing to ForestForesight

This workflow guides contributors on how to address **[confirmed]** issues in alignment with our branching, testing, and styling strategies.

## Table of Contents
1. [Valid Issues](#valid-issues)
2. [Workflow](#workflow)
   - [1. Assign the Issue](#1-assign-the-issue)
   - [2. Fork the Repository](#2-fork-the-repository)
   - [3. Create a Branch from the Issue](#3-create-a-branch-from-the-issue)
   - [4. Develop and Test Your Changes](#4-develop-and-test-your-changes)
   - [5. Create a Pull Request (PR)](#5-create-a-pull-request-pr)
   - [6. Review and Respond](#6-review-and-respond)
   - [7. Approval and Merge](#7-approval-and-merge)

---

## Valid Issues
Only issues with the label **[confirmed]** should be picked up and fixed. If you are interested in a new feature or bug fix, please check if there is already an issue created. If not, create a new issue, and wait for it to be labeled **[confirmed]** by maintainers before proceeding.

## Workflow

### 1. Assign the Issue
   - Find an open, **[confirmed]** issue that you would like to work on. Assign the issue to yourself so other contributors know you are working on it.

### 2. Fork the Repository
   - If you have not already, fork the repository to create a copy under your GitHub account. This will allow you to make changes without direct access to the main repository.

### 3. Create a Branch from the Issue
   - In GitHub, navigate to the issue and go to the **Development** section.
   - Select **Create a branch**, and choose your forked repository as the location for the branch.
   - Here you will have two options:
     - **Create a new branch** for the issue. See our [Branching Strategy](https://forestforesight.atlassian.net/wiki/spaces/EWS/overview?homepageId=32961) section for more details.
     - Use the **default branch (develop)**.

### 4. Develop and Test Your Changes
   - After making the necessary changes in your branch, follow our [Testing Strategy](https://forestforesight.atlassian.net/wiki/spaces/EWS/overview?homepageId=32961) to ensure your changes are covered by unit tests.
   - Ensure code quality by running the `styler` and `lintr` checks. You can follow the instructions in the [Tools](https://forestforesight.atlassian.net/wiki/spaces/EWS/overview?homepageId=32961) section to use `styler` and `lintr`.
   - Confirm that all code meets the project’s standards before submitting.

### 5. Create a Pull Request (PR)
   - Once your changes are complete and tested, open a PR from your forked repository’s branch to the main repository’s `develop` branch.
   - Ensure your PR:
     - Clearly references the issue it addresses.
     - Follows our testing and styling guidelines.
     - Is ready for review by maintainers.

### 6. Review and Respond
   - Wait for maintainers to review your PR. They will either approve the PR if it meets all criteria or request changes if adjustments are needed.
   - If changes are requested:
     - Respond to comments and make necessary updates at your earliest convenience.
     - Push the updates to your branch in the forked repository; these changes will be reflected in the PR.

### 7. Approval and Merge
   - Once your PR is approved, a maintainer will merge it into the `develop` branch in the main repository.
   - After merging, the issue will be closed, and your contribution will become part of the project.

Thank you for contributing and helping us improve this project! Please ensure all contributions align with our guidelines to maintain quality and consistency across the codebase. 
**Note:** Our guidelines are continually being improved, so please check back regularly for any updates.
