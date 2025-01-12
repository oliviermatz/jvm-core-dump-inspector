/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */

package org.netbeans.lib.profiler.heap;

/**
 *
 * @author Tomas Hurka
 */
public class LoadClassSegment extends TagBounds {
    //~ Instance fields ----------------------------------------------------------------------------------------------------------

    HprofHeap hprofHeap;
    final int classIDOffset;
    final int classSerialNumberOffset;
    final int lengthOffset;
    final int nameStringIDOffset;
    final int stackTraceSerialOffset;
    final int timeOffset;

    //~ Constructors -------------------------------------------------------------------------------------------------------------

    LoadClassSegment(HprofHeap heap, long start, long end) {
        super(HprofHeap.LOAD_CLASS, start, end);

        int idSize = heap.dumpBuffer.getIDSize();
        hprofHeap = heap;
        timeOffset = 1;
        lengthOffset = timeOffset + 4;
        classSerialNumberOffset = lengthOffset + 4;
        classIDOffset = classSerialNumberOffset + 4;
        stackTraceSerialOffset = classIDOffset + idSize;
        nameStringIDOffset = stackTraceSerialOffset + 4;
    }

    //~ Methods ------------------------------------------------------------------------------------------------------------------

    LoadClass getClassByID(long classObjectID) {
        long[] offset = new long[] { startOffset };

        while (offset[0] < endOffset) {
            long start = offset[0];
            long classID = readLoadClassID(offset);

            if (classID == classObjectID) {
                return new LoadClass(this, start);
            }
        }

        return null;
    }

    LoadClass getClassBySerialNumber(int classSerialNumber) {
        long[] offset = new long[] { startOffset };

        while (offset[0] < endOffset) {
            long start = offset[0];
            int serial = readLoadClassSerialNumber(offset);

            if (serial == classSerialNumber) {
                return new LoadClass(this, start);
            }
        }

        return null;
    }
    
    void setLoadClassOffsets() {
        ClassDumpSegment classDumpSegment = hprofHeap.getClassDumpSegment();
        long[] offset = new long[] { startOffset };

        while (offset[0] < endOffset) {
            long start = offset[0];
            long classID = readLoadClassID(offset);
            ClassDump classDump = classDumpSegment.getClassDumpByID(classID);

            if (classDump != null) {
                classDump.setClassLoadOffset(start);
            }
        }
    }

    private HprofByteBuffer getDumpBuffer() {
        HprofByteBuffer dumpBuffer = hprofHeap.dumpBuffer;

        return dumpBuffer;
    }

    private int readLoadClassSerialNumber(long[] offset) {
        long start = offset[0];

        if (hprofHeap.readTag(offset) != HprofHeap.LOAD_CLASS) {
            return 0;
        }

        return getDumpBuffer().getInt(start + classSerialNumberOffset);
    }
    
    private long readLoadClassID(long[] offset) {
        long start = offset[0];

        if (hprofHeap.readTag(offset) != HprofHeap.LOAD_CLASS) {
            return 0;
        }

        return getDumpBuffer().getID(start + classIDOffset);
    }
}
