#!/usr/bin/env python3
"""
Quarto Project Validation Script

This script validates the structure and syntax of Quarto .qmd files
before rendering. It catches common errors that would cause build failures.

Usage:
    python3 scripts/validate_quarto.py

Exit codes:
    0 - All validations passed
    1 - Errors found (build will likely fail)
    2 - Warnings only (build should succeed)
"""

import re
import sys
import os
import glob
from pathlib import Path
from typing import List, Tuple, Dict


class Colors:
    """ANSI color codes for terminal output"""
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'


def validate_qmd_file(filepath: str) -> Tuple[List[str], List[str]]:
    """
    Validate a single .qmd file

    Returns:
        Tuple of (errors, warnings)
    """
    errors = []
    warnings = []

    with open(filepath, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    content = ''.join(lines)

    # Track state
    in_code_block = False
    div_stack = []

    for i, line in enumerate(lines, 1):
        stripped = line.strip()

        # Track code blocks
        if stripped.startswith('```'):
            in_code_block = not in_code_block

            # Check for old-style chunk options
            if stripped.startswith('```{r') and not in_code_block:
                # Check if options are in header
                if ',' in line or ('=' in line and not stripped.startswith('```{r}')):
                    # Exclude eval: !expr which is valid
                    if '!expr' not in line:
                        warnings.append(
                            f"Line {i}: Old-style chunk options in header. "
                            "Use #| prefix instead"
                        )
            continue

        # Skip lines inside code blocks
        if in_code_block:
            continue

        # Check for div blocks (outside code blocks)
        if stripped.startswith(':::'):
            if '{' in stripped:  # Opening div
                div_stack.append(i)
            elif stripped == ':::':  # Closing div
                if div_stack:
                    div_stack.pop()
                else:
                    errors.append(
                        f"Line {i}: Closing ::: without matching opening"
                    )

    # Check for unclosed divs
    if div_stack:
        errors.append(
            f"Unclosed div blocks starting at lines: {div_stack}"
        )

    # Check for unclosed code blocks
    code_block_count = content.count('```')
    if code_block_count % 2 != 0:
        errors.append(
            f"Unclosed code block (found {code_block_count} backticks)"
        )

    # Check for old-style cross-references
    old_refs = re.findall(r'\\@ref\([^)]+\)', content)
    if old_refs:
        unique_refs = set(old_refs[:5])  # Show up to 5 unique examples
        warnings.append(
            f"Found {len(old_refs)} old-style cross-references. "
            f"Examples: {', '.join(unique_refs)}. "
            "Use @sec-, @fig-, @tbl-, @eq- instead"
        )

    # Check for common package name typos
    typos = {
        'ur ca': 'urca',
        'lm test': 'lmtest',
        'sand wich': 'sandwich',
    }

    for typo, correct in typos.items():
        if typo in content:
            errors.append(
                f"Found '{typo}' - should be '{correct}' (package name typo)"
            )

    # Check for child chunk references
    child_refs = re.findall(r'child:\s*["\']([^"\']+)["\']', content)
    for child_file in child_refs:
        if not os.path.exists(child_file):
            errors.append(
                f"Child file not found: {child_file}"
            )

    return errors, warnings


def validate_yaml(filepath: str) -> List[str]:
    """Validate YAML file syntax"""
    errors = []

    try:
        import yaml
        with open(filepath, 'r') as f:
            yaml.safe_load(f)
    except ImportError:
        errors.append(
            "PyYAML not installed. Run: pip install pyyaml"
        )
    except yaml.YAMLError as e:
        errors.append(f"YAML syntax error: {e}")

    return errors


def check_project_structure() -> Tuple[List[str], List[str]]:
    """Check overall project structure"""
    errors = []
    warnings = []

    # Check _quarto.yml exists
    if not os.path.exists('_quarto.yml'):
        errors.append("_quarto.yml not found")
        return errors, warnings

    # Validate _quarto.yml
    yaml_errors = validate_yaml('_quarto.yml')
    errors.extend(yaml_errors)

    if yaml_errors:
        return errors, warnings

    # Check chapters
    try:
        import yaml
        with open('_quarto.yml', 'r') as f:
            config = yaml.safe_load(f)

        chapters = config.get('book', {}).get('chapters', [])
        for chapter in chapters:
            if not os.path.exists(chapter):
                errors.append(f"Chapter file not found: {chapter}")

        # Check theme files
        formats = config.get('format', {})
        html_format = formats.get('html', {})
        theme = html_format.get('theme', {})

        if isinstance(theme, dict):
            for mode, theme_files in theme.items():
                if isinstance(theme_files, list):
                    for tf in theme_files:
                        if tf.endswith('.scss') and not os.path.exists(tf):
                            warnings.append(f"Theme file not found: {tf}")

        # Check bibliography
        bib_file = config.get('bibliography')
        if bib_file and not os.path.exists(bib_file):
            errors.append(f"Bibliography file not found: {bib_file}")
        elif not bib_file:
            warnings.append("No bibliography configured in _quarto.yml")

    except Exception as e:
        errors.append(f"Error checking project structure: {e}")

    return errors, warnings


def print_results(
    all_errors: Dict[str, List[str]],
    all_warnings: Dict[str, List[str]],
    project_errors: List[str],
    project_warnings: List[str]
) -> int:
    """Print validation results with colors"""

    total_errors = sum(len(e) for e in all_errors.values()) + len(project_errors)
    total_warnings = sum(len(w) for w in all_warnings.values()) + len(project_warnings)

    print(f"\n{Colors.BOLD}{'='*70}{Colors.ENDC}")
    print(f"{Colors.BOLD}{Colors.HEADER}  Quarto Project Validation Results{Colors.ENDC}")
    print(f"{Colors.BOLD}{'='*70}{Colors.ENDC}\n")

    # Project-level issues
    if project_errors:
        print(f"{Colors.FAIL}{Colors.BOLD}❌ PROJECT ERRORS:{Colors.ENDC}\n")
        for err in project_errors:
            print(f"  {Colors.FAIL}✗{Colors.ENDC} {err}")
        print()

    if project_warnings:
        print(f"{Colors.WARNING}{Colors.BOLD}⚠️  PROJECT WARNINGS:{Colors.ENDC}\n")
        for warn in project_warnings:
            print(f"  {Colors.WARNING}!{Colors.ENDC} {warn}")
        print()

    # File-level issues
    if all_errors:
        print(f"{Colors.FAIL}{Colors.BOLD}❌ FILE ERRORS:{Colors.ENDC}\n")
        for file, errors in sorted(all_errors.items()):
            print(f"{Colors.BOLD}  {file}:{Colors.ENDC}")
            for err in errors:
                print(f"    {Colors.FAIL}✗{Colors.ENDC} {err}")
            print()

    if all_warnings:
        print(f"{Colors.WARNING}{Colors.BOLD}⚠️  FILE WARNINGS:{Colors.ENDC}\n")
        for file, warnings in sorted(all_warnings.items()):
            print(f"{Colors.BOLD}  {file}:{Colors.ENDC}")
            for warn in warnings:
                print(f"    {Colors.WARNING}!{Colors.ENDC} {warn}")
            print()

    # Summary
    print(f"{Colors.BOLD}{'='*70}{Colors.ENDC}")

    if total_errors == 0 and total_warnings == 0:
        print(f"{Colors.OKGREEN}{Colors.BOLD}✅ All validations passed!{Colors.ENDC}")
        print(f"{Colors.OKGREEN}   No errors or warnings found.{Colors.ENDC}")
        return 0
    elif total_errors == 0:
        print(f"{Colors.OKCYAN}{Colors.BOLD}✓ Validation passed with warnings{Colors.ENDC}")
        print(f"{Colors.WARNING}   {total_warnings} warning(s) found{Colors.ENDC}")
        print(f"{Colors.OKCYAN}   Build should succeed, but review warnings{Colors.ENDC}")
        return 2
    else:
        print(f"{Colors.FAIL}{Colors.BOLD}✗ Validation failed{Colors.ENDC}")
        print(f"{Colors.FAIL}   {total_errors} error(s) found{Colors.ENDC}")
        if total_warnings > 0:
            print(f"{Colors.WARNING}   {total_warnings} warning(s) found{Colors.ENDC}")
        print(f"{Colors.FAIL}   Build will likely fail. Fix errors before rendering.{Colors.ENDC}")
        return 1


def main():
    """Main validation function"""

    # Check if running in project root
    if not os.path.exists('_quarto.yml'):
        print(f"{Colors.FAIL}Error: _quarto.yml not found.{Colors.ENDC}")
        print("Run this script from the project root directory.")
        sys.exit(1)

    print(f"\n{Colors.HEADER}{Colors.BOLD}🔍 Validating Quarto Project...{Colors.ENDC}\n")

    # Validate project structure
    project_errors, project_warnings = check_project_structure()

    # Find all .qmd files
    qmd_files = glob.glob('*.qmd')
    qmd_files.sort()

    print(f"Found {len(qmd_files)} .qmd files to validate:")
    for qmd in qmd_files:
        size = os.path.getsize(qmd)
        print(f"  • {qmd:<30} ({size:,} bytes)")

    # Validate each file
    all_errors = {}
    all_warnings = {}

    for qmd in qmd_files:
        errors, warnings = validate_qmd_file(qmd)
        if errors:
            all_errors[qmd] = errors
        if warnings:
            all_warnings[qmd] = warnings

    # Print results
    exit_code = print_results(
        all_errors, all_warnings,
        project_errors, project_warnings
    )

    print(f"{Colors.BOLD}{'='*70}{Colors.ENDC}\n")

    sys.exit(exit_code)


if __name__ == '__main__':
    main()
