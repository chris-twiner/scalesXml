# Scales XML

## {{project_version()}}

??? coverage "Coverage"

    <table>
    <tr>
        <td>Statement</td>
        <td class="coveragePercent">{{statement_coverage()}}:material-percent-outline:</td>
        <td>Branch</td>
        <td class="coveragePercent">{{branch_coverage()}}:material-percent-outline:</td>
    </tr>
    </table>

Scales XML is an alternative XML library for Scala, its design started with the question - _what if the structure of XML was separated from its contents?_.

The answer for XML tends naturally to trees and zippers, enabling a combined model for both XML Tree handling and XML Event handling.  This allows opportunities for saving memory usage and increasing performance.

The design aims of Scales Xml also target correctness first, an Iteratee based processing for Pull, an XPath like syntax for querying and manipulation and deep support for JAXP.  See the release notes for [Iteratee upgrade information](Getting_Started/release_notes/0.6.0.md#scalaz-iteratee).

:new:{.pulseABit} Scalaz Iteratee library usage along with a necessary upgrade guide in the [release notes](Getting_Started/release_notes/0.6.0.md) 

__The main focus areas are__

* Correctness
* Simplified and consistent model - shared between push and pull
* Allowing full re-use of the data model - QNames, Elems etc all reusable
<br></br>
* Fast Internal XPath Api
* Full XPath1.0 String evaluation support via Jaxen
* Iteratee based xml Pull handling - you choose what, when and how to process
<br></br>
* Non-blocking IO xml Pull processing via Aalto 
* High Level of JAXP Integration - Validation, TrAX and Serialisation (when needed)
* Performance - fast with low memory usage