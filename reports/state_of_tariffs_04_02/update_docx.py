"""
Update state_of_tariffs.docx with new numbers from data_download.xlsx.
Preserves all formatting by doing targeted run-level text replacements.
"""

from docx import Document
import os
import shutil

DIR = os.path.dirname(os.path.abspath(__file__))
DOCX_PATH = os.path.join(DIR, 'state_of_tariffs.docx')
BACKUP_PATH = os.path.join(DIR, 'state_of_tariffs_backup.docx')

# Back up original
shutil.copy2(DOCX_PATH, BACKUP_PATH)

doc = Document(DOCX_PATH)

changes = []

for i, para in enumerate(doc.paragraphs):
    full_text = para.text

    # =========================================================================
    # P3: Summary - Current Tariff Rate
    # =========================================================================
    if 'average effective tariff rate stands at 10.6%' in full_text:
        for run in para.runs:
            if '10.6%' in run.text:
                run.text = run.text.replace('10.6%', '11.0%')
                run.text = run.text.replace('7.7%', '8.2%')
            if '9.3% and 6.7%' in run.text:
                run.text = run.text.replace('9.3%', '9.6%').replace('6.7%', '7.1%')
        changes.append(f'P{i}: Updated ETR summary (10.6->11.0, 7.7->8.2, 9.3->9.6, 6.7->7.1)')

    # =========================================================================
    # P4: Summary - Prices & Distributional
    # =========================================================================
    elif 'between 0.4% and 0.5%, representing' in full_text:
        runs = para.runs
        for j, run in enumerate(runs):
            # Non-italic part: price range and HH costs
            if 'between 0.4% and 0.5%' in run.text:
                run.text = run.text.replace('between 0.4% and 0.5%', 'between 0.5% and 0.6%')
            # "$620" is split: R16="$6", R17="20"
            if run.text == '20' and j > 0 and runs[j - 1].text == '$6':
                run.text = '50'  # $620 -> $650
            if ' and $740' in run.text:
                run.text = run.text.replace('$740', '$780')
            # Italic part: perm scenario
            if 'between 0.8% and 0.9%' in run.text:
                run.text = run.text.replace('between 0.8% and 0.9%', 'between 0.8% and 1.0%')
            # "$1,100" split: R24="100"
            if run.text == '100' and run.italic and j > 0 and '$1,' in runs[j - 1].text:
                run.text = '130'  # $1,100 -> $1,130
            # "$1,300" split: R26="300"
            if run.text == '300' and run.italic and j > 0 and '$1,' in runs[j - 1].text:
                run.text = '340'  # $1,300 -> $1,340
        changes.append(f'P{i}: Updated price summary (0.4/0.5->0.5/0.6, $620->$650, $740->$780, 0.9->1.0, $1100->$1130, $1300->$1340)')

    # =========================================================================
    # P5: Summary - Macro Effects
    # =========================================================================
    elif 'persistently 0.' in full_text and 'billion annually' in full_text:
        runs = para.runs
        for j, run in enumerate(runs):
            # "$25" is split: R3 ends with "$2", R4="5"
            if run.text == '5' and j > 0 and runs[j - 1].text.endswith('$2'):
                run.text = '7'  # $25 -> $27
            # Italic: "grows by half" -> "grows by about two-thirds"
            if 'grows by half' in run.text:
                run.text = run.text.replace('grows by half', 'grows by about two-thirds')
        changes.append(f'P{i}: Updated macro summary ($25->$27, grows by half->about two-thirds)')

    # =========================================================================
    # P6: Summary - Sectoral Effects
    # =========================================================================
    elif 'manufacturing output expands by' in full_text:
        for run in para.runs:
            if 'expands by 0.6%' in run.text:
                run.text = run.text.replace('expands by 0.6%', 'expands by 0.7%')
            if 'contracts by 1.9%' in run.text:
                run.text = run.text.replace('contracts by 1.9%', 'contracts by 2.0%')
            if 'declines by 0.7%' in run.text:
                run.text = run.text.replace('declines by 0.7%', 'declines by 0.8%')
        changes.append(f'P{i}: Updated sector summary (mfg 0.6->0.7, const 1.9->2.0, mining 0.7->0.8)')

    # =========================================================================
    # P7: Summary - Fiscal Effects
    # =========================================================================
    elif 'raise about $1.1 trillion over 2026' in full_text:
        for run in para.runs:
            if '$1.5 trillion' in run.text:
                run.text = run.text.replace('$1.5 trillion', '$1.6 trillion')
        changes.append(f'P{i}: Updated fiscal summary ($1.5T->$1.6T)')

    # =========================================================================
    # P23: ETR Detail - Pre-substitution paragraph
    # =========================================================================
    elif '8.6 percentage point increase' in full_text:
        for run in para.runs:
            run.text = run.text.replace('8.6 percentage point', '9.0 percentage point')
            run.text = run.text.replace('10.6%', '11.0%')
        changes.append(f'P{i}: Updated ETR detail pre-sub (8.6->9.0 pp, 10.6->11.0)')

    # =========================================================================
    # P24: ETR Detail - Post-substitution paragraph
    # =========================================================================
    elif '7.3 percentage point increase' in full_text:
        for run in para.runs:
            run.text = run.text.replace('7.3 percentage point', '7.6 percentage point')
            run.text = run.text.replace('9.3%', '9.6%')
            run.text = run.text.replace('7.7%', '8.2%')
            run.text = run.text.replace('6.7%', '7.1%')
        changes.append(f'P{i}: Updated ETR detail post-sub (7.3->7.6, 9.3->9.6, 7.7->8.2, 6.7->7.1)')

    # =========================================================================
    # P29: Price Detail - Main paragraph
    # =========================================================================
    elif 'increase in consumer prices of 0.9%' in full_text:
        for run in para.runs:
            run.text = run.text.replace('of 0.9%', 'of 1.0%')
            run.text = run.text.replace('about 0.5%', 'about 0.6%')
            run.text = run.text.replace('$1,298', '$1,338')
            run.text = run.text.replace('$740', '$780')
        changes.append(f'P{i}: Updated price detail (0.9->1.0, 0.5->0.6, $1298->$1338, $740->$780)')

    # =========================================================================
    # P30: Price Detail - Post-sub paragraph
    # =========================================================================
    elif 'post-substitution price increase settles at' in full_text:
        for run in para.runs:
            run.text = run.text.replace('at 0.4%', 'at 0.5%')
            run.text = run.text.replace('$617', '$648')
            run.text = run.text.replace('$1,099', '$1,130')
        changes.append(f'P{i}: Updated price post-sub (0.4->0.5, $617->$648, $1099->$1130)')

    # =========================================================================
    # P32: Macro Detail
    # =========================================================================
    elif '0.09% to 0.16% smaller' in full_text:
        for run in para.runs:
            if '0.09% to 0.16%' in run.text:
                run.text = run.text.replace('0.09%', '0.10%')
        changes.append(f'P{i}: Updated macro detail (0.09->0.10)')

    # =========================================================================
    # P41: Fiscal Detail - Expires
    # =========================================================================
    elif 'total about $86 billion' in full_text:
        for run in para.runs:
            run.text = run.text.replace('$86 billion', '$90 billion')
        changes.append(f'P{i}: Updated fiscal detail ($86B->$90B)')

    # =========================================================================
    # P42: Fiscal Detail - Extended
    # =========================================================================
    elif 'the dynamic score would be roughly $1.5 trillion' in full_text:
        for run in para.runs:
            if '$1.5 trillion' in run.text:
                run.text = run.text.replace('$1.5 trillion', '$1.6 trillion')
        changes.append(f'P{i}: Updated fiscal extended ($1.5T->$1.6T)')

    # =========================================================================
    # P46: Distribution Detail
    # =========================================================================
    elif 'first decile is about' in full_text and 'times that of the top' in full_text:
        runs = para.runs
        for j, run in enumerate(runs):
            # D10 temp: "0.3%" -> "0.4%" (R5 = "3" after R4 = "% versus 0.")
            if run.text == '3' and j > 0 and '% versus 0.' in runs[j - 1].text:
                # Only change the first occurrence (temp scenario)
                run.text = '4'

            # D1 cost temp: "$400" -> "$430" (R13 = "400")
            if run.text == '400' and j > 0 and '$' in runs[j - 1].text:
                run.text = '430'

            # D10 cost temp: "$1,700" -> "$1,810" (R15 = "1,700")
            if run.text == '1,700':
                run.text = '1,810'

            # D1 cost perm: "$700" -> "$740" (R17 = "700")
            if run.text == '700' and j > 0 and '$' in runs[j - 1].text:
                run.text = '740'

            # D10 cost perm: "$3,000" -> "$3,100" (R18 = " and $3,", R19 = "000")
            if run.text == '000' and j > 0 and '$3,' in runs[j - 1].text:
                run.text = '100'

        changes.append(f'P{i}: Updated distribution (D10: 0.3->0.4, $400->$430, $1700->$1810, $700->$740, $3000->$3100)')


doc.save(DOCX_PATH)

print(f'Updated {DOCX_PATH}')
print(f'Backup saved to {BACKUP_PATH}')
print(f'\nChanges made ({len(changes)}):')
for c in changes:
    print(f'  {c}')
