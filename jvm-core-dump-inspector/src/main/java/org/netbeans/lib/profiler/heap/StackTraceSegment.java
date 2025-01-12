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

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Tomas Hurka
 */
public class StackTraceSegment extends TagBounds {

    private static final int SERIALNUM_DIV = 16;
    //~ Instance fields ----------------------------------------------------------------------------------------------------------

    HprofHeap hprofHeap;
    final int threadSerialNumberOffset;
    final int stackTraceSerialNumberOffset;
    final int lengthOffset;
    final int framesListOffset;
    final int numberOfFramesOffset;
    final int timeOffset;
    private Map serialNumToStackTrace;

    //~ Constructors -------------------------------------------------------------------------------------------------------------

    StackTraceSegment(HprofHeap heap, long start, long end) {
        super(HprofHeap.STACK_TRACE, start, end);

        hprofHeap = heap;
        timeOffset = 1;
        lengthOffset = timeOffset + 4;
        stackTraceSerialNumberOffset = lengthOffset + 4;
        threadSerialNumberOffset = stackTraceSerialNumberOffset + 4;
        numberOfFramesOffset = threadSerialNumberOffset + 4;
        framesListOffset = numberOfFramesOffset + 4;
    }

    //~ Methods ------------------------------------------------------------------------------------------------------------------

    StackTrace getStackTraceBySerialNumber(long stackTraceSerialNumber) {
        Long initialOffset;
        long[] offset;
        
        initSerialNumToFrame();
        initialOffset = (Long) serialNumToStackTrace.get(new Long(stackTraceSerialNumber/SERIALNUM_DIV));
        if (initialOffset == null) {
            initialOffset = new Long(startOffset);
        }
        offset = new long[] { initialOffset.longValue() };
        while (offset[0] < endOffset) {
            long start = offset[0];
            long serialNumber = readStackTraceTag(offset);

            if (serialNumber == stackTraceSerialNumber) {
                return new StackTrace(this, start);
            }
        }
        return null;
    }

    private HprofByteBuffer getDumpBuffer() {
        HprofByteBuffer dumpBuffer = hprofHeap.dumpBuffer;

        return dumpBuffer;
    }

    private int readStackTraceTag(long[] offset) {
        long start = offset[0];

        if (hprofHeap.readTag(offset) != HprofHeap.STACK_TRACE) {
            return 0;
        }
        return getDumpBuffer().getInt(start + stackTraceSerialNumberOffset);
    }

    private synchronized void initSerialNumToFrame() {
        if (serialNumToStackTrace == null) {
            long[] offset = new long[] { startOffset };

            serialNumToStackTrace = new HashMap();
            while (offset[0] < endOffset) {
                long start = offset[0];
                long serialNumber = readStackTraceTag(offset);
                Long serialNumberMask = new Long(serialNumber/SERIALNUM_DIV);
                Long minOffset = (Long) serialNumToStackTrace.get(serialNumberMask);
                
                if (minOffset == null || minOffset > start) {
                    serialNumToStackTrace.put(serialNumberMask, new Long(start));
                }
            }
//            System.out.println("serialNumToStackTrace size:"+serialNumToStackTrace.size());
        }
    }
}
