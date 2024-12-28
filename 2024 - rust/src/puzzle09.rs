use crate::helper::GenResult;
use colored::Colorize;
use log::{debug, info};
use std::collections::LinkedList;
use std::fmt::{Debug, Write};
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use utf8_chars::BufReadCharsExt;

pub fn run(input_path: &Path) -> GenResult<()> {
    let mut fs = parse_input(input_path)?;

    let segments_v2 = fs.unformatted.iter().cloned().collect::<Vec<_>>();

    info!("Start: {:?}", fs);
    defrag(&mut fs);
    info!("End: {:?}", fs);
    let part1_sum = checksum(&fs.formatted);
    info!("Part 1 Checksum: {}", part1_sum);

    fill_v2(&segments_v2);

    Ok(())
}

#[derive(Clone)]
struct Segment {
    id: usize,
    length: usize,
    gap: usize,
}

impl Debug for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[a{}]", self.id.to_string().green())?;
        for _ in 0..self.length {
            f.write_char('#')?;
        }
        for _ in 0..self.gap {
            write!(f, "{}", ".".bright_black())?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct Filesystem {
    formatted: Vec<Segment>,
    unformatted: LinkedList<Segment>,
}

fn parse_input(input_path: &Path) -> GenResult<Filesystem> {
    let file = File::open(input_path)?;
    let mut reader = BufReader::new(file);
    let mut itr = reader.chars().map(Result::unwrap).filter(|c| *c != '\n');
    let mut out = LinkedList::new();
    let mut id = 0;

    while let Some(c) = itr.next() {
        let length = { c.to_digit(10).ok_or(format!("not a digit: {}", c))? as usize };
        let gap = if let Some(c2) = itr.next() {
            c2.to_digit(10).ok_or(format!("not a digit: {}", c2))? as usize
        } else {
            0
        };
        out.push_back(Segment { id, length, gap });
        id += 1;
    }

    Ok(Filesystem {
        formatted: Vec::new(),
        unformatted: out,
    })
}

fn defrag(fs: &mut Filesystem) {
    while let Some(mut segment) = fs.unformatted.pop_front() {
        let gap = segment.gap;

        // move the segment to the formatted section
        segment.gap = 0;
        fs.formatted.push(segment);

        // handle the remaining gap by pulling items from the back of the filesystem
        fill_gap(gap, fs);
    }
}

fn fill_gap(gap: usize, fs: &mut Filesystem) {
    if gap > 0 {
        if let Some(mut segment) = fs.unformatted.pop_back() {
            let length = segment.length;
            if length <= gap {
                // whole segment fits in the gap
                segment.gap = 0;
                fs.formatted.push(segment);
                // recurse the fill what remains of the gap
                fill_gap(gap - length, fs);
            } else {
                // segment is too big for the gap;
                // add a slice of this segment to the formatted region,
                // and return the rest to the back of the unformatted region
                fs.formatted.push(Segment {
                    id: segment.id,
                    length: gap,
                    gap: 0,
                });
                segment.length -= gap;
                fs.unformatted.push_back(segment);
            }
        }
    }
}

fn checksum(segments: &Vec<Segment>) -> usize {
    let mut i = 0;
    let mut accum = 0;
    for seg in segments.iter() {
        for d in 0..seg.length {
            accum += (i + d) * seg.id;
        }
        i += seg.length;
    }
    accum
}

#[derive(Debug)]
struct Gap {
    offset: usize,
    width: usize,
}

#[derive(Debug)]
struct SegmentV2 {
    id: usize,
    offset: usize,
    width: usize,
}

fn fill_v2(segments: &Vec<Segment>) {
    // represent the filesystem as an arbitrarily-ordered collection of SegmentV2,
    // where each item tracks its own `offset` from the start of the filesystem,
    let mut segments_v2 = {
        let mut out = Vec::new();

        let mut accum_offset = 0;
        for seg in segments {
            out.push(SegmentV2 {
                id: seg.id,
                offset: accum_offset,
                width: seg.length,
            });
            accum_offset += seg.length + seg.gap;
        }

        out
    };

    info!("segments v2: {:?}", segments_v2);

    // Also represent the gaps between items in the filesystem. For the part 2
    // algorithm, each segment will need to search the `gaps` for one wide enough.
    let mut gaps = {
        let mut accum_offset = 0;
        let mut out = Vec::new();
        for seg in segments {
            accum_offset += seg.length;
            if seg.gap > 0 {
                out.push(Gap {
                    offset: accum_offset,
                    width: seg.gap,
                });
            }
            accum_offset += seg.gap;
        }
        out
    };

    info!("gaps: {:?}", gaps);

    for seg in segments_v2.iter_mut().rev() {
        if let Some(gap_index) = gaps
            .iter_mut()
            .take_while(|g| g.offset < seg.offset)
            .position(|g| g.width >= seg.width)
        {
            let gap = &mut gaps[gap_index];
            // the segment can be moved into the gap
            debug!("{:?} can fit in {:?}", seg, gap);
            if gap.width > seg.width {
                debug!("gap shrinks by {}", seg.width);
                seg.offset = gap.offset;
                gap.offset += seg.width;
                gap.width -= seg.width;
            } else {
                debug!("gap closed");
                seg.offset = gap.offset;
                gaps.remove(gap_index);
            }
        }
    }

    segments_v2.sort_by_key(|seg| seg.offset);
    info!("final segments: {:?}", segments_v2);

    let checksum = {
        let mut accum = 0;
        for seg in segments_v2 {
            for j in 0..seg.width {
                let i = seg.offset + j;
                accum += i * seg.id;
            }
        }
        accum
    };
    info!("part 2 checksum: {}", checksum);

}

// fn find_gap(segments: &Vec<Segment>, width: usize, max_offset: usize) -> usize {}
