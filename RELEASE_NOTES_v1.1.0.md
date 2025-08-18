# Release v1.1.0 - Project Management Support

## New Features

### Project Management Integration
- **Full Linear Project Support**: View, create, and manage Linear projects directly from Emacs
- **Project-Issue Association**: Link issues to projects and navigate between them 
- **Org-mode Integration**: Projects are displayed with proper org-mode formatting for better readability
- **Interactive Project Selection**: When creating issues, you can now select from available projects in your workspace

## Bug Fixes
- Fixed remaining Elisp compilation warnings for cleaner package installation
- Resolved function naming inconsistencies

## Other Changes
- Added Codeberg as an alternative repository mirror
- Minor code cleanup and improvements

## Installation

For new users:
```elisp
(use-package linear-emacs
  :straight (:host github :repo "anegg0/linear-emacs")
  :config
  (setq linear-api-key "your-api-key-here"))
```

**Full Changelog**: https://github.com/anegg0/linear-emacs/compare/v1.0.1...v1.1.0
